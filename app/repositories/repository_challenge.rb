# This file only has encoded MongoDB queries, which can become quite long.
# So let's disable the Rubocop warnings regarding length of the module,
# since there's little point in breaking these down.
#
# rubocop:disable Metris/ModuleLength

module RepositoryChallenge

  # Challenge.collection return a Mongo::Collection
  # see http://api.mongodb.com/ruby/current/Mongo/Collection.html
  #
  # Challenge.collection.aggregate return an Aggregation
  # see http://api.mongodb.com/ruby/current/Mongo/Collection.html#aggregate-instance_method
  #
  # args is the aggregation pipeline:
  # an array of Hash, starting with an operator
  # For pipeline definition see https://docs.mongodb.com/manual/core/aggregation-pipeline/
  # For operators see https://docs.mongodb.com/manual/reference/operator/aggregation-pipeline/
  #
  # Returns a Mongo::Collection::View::Aggregation
  # see http://api.mongodb.com/ruby/current/Mongo/Collection/View/Aggregation.html
  #
  # Example :
  # RepositoryChallenge.entry_aggregate({"$count": 'challenge_count'}).to_a
  # => [{"challenge_count"=>102}]
  def self.entry_aggregate(*args)
    Entry.collection.aggregate(args.flatten)
  end

  def self.challenge_aggregate(*args)
    Challenge.collection.aggregate(args.flatten)
  end

  def self.score_query
    [
      {
        "$project": {
          "_id": 1,
          "title": 1,
          "description": 1,
          "created_at": 1,
        },
      },
      { "$sort": { "created_at": -1 } },
    ]
  end

  # List of pipeline operators for .entry_aggregate
  # to paginate every query
  # see https://docs.mongodb.com/manual/reference/operator/aggregation/project/
  #
  # Example:
  # RepositoryChallenge.entry_aggregate(
  #   [{ '$project': {title: 1}}],
  #   RepositoryChallenge.paginate(per_page: 2, page: 1)
  # ).to_a
  # => [
  #   {"_id"=>BSON::ObjectId('5af761d2cfa4a41a1957b464'), "title"=>"Foo"},
  #   {"_id"=>BSON::ObjectId('5af761d4cfa4a41a1957d3a5'), "title"=>"Bar"}
  # ]
  def self.paginate(per_page:, page:)
    [
      { "$skip": (per_page * (page-1))  },
      { "$limit": per_page },
    ]
  end

  def self.paginate_home_page(per_page:, page:)
    challenge_aggregate(
      score_query,
      paginate(per_page: per_page, page: page)
    )
  end

  # Reference query
  def self.best_score_per_user(challenge_id)
    [
      { "$match": { "challenge_id": challenge_id } },
      { "$sort": { "score": 1, "created_at": 1 } },
      {
        '$group': {
          "_id": '$user_id',
          "entry_id": { "$first": '$_id' },
          "user_id": { "$first": '$user_id' },
          "created_at": { "$first": '$created_at' },
          "script": { "$first": '$script' },
          "comments": { "$first": '$comments' },
          "min_score": { "$first": '$score'}
        }
      },
      { "$sort": { "min_score": 1, "created_at": 1 } },
    ]
  end

  # For any given query, return the number of entries
  # return nil when no entries
  #
  # Note: do not use distinct + length on big collection
  # because it loads a big array. prefere 'sum'
  # Example to avoid :
  # Challenge.collection.distinct('entries.user_id').length
  def self.sum_lines(*args)
    result = entry_aggregate(
      args.concat([
        {
          '$group': {
            "_id": 1,
            "sum_lines": { "$sum": 1 },
          }
        }
      ]),
    ).first
    result && result['sum_lines']
  end

  def self.paginate_leaderboard(challenge_id:, per_page:, page:)
    entry_aggregate(
      best_score_per_user(challenge_id),
      paginate(per_page: per_page, page: page)
    )
  end

  # Return the worst score for a given challenge
  # Still need to group by user in case a user has a worst score,
  # but not visible solution.
  #
  # Return nil when no entries
  def self.worst_score(challenge_id)
    result = entry_aggregate(
      best_score_per_user(challenge_id),
      { "$sort": { "min_score": -1 } },
      { "$limit": 1 },
    ).first
    result && result['min_score']
  end

  # Return the next lowest score bellow a given score
  # Still need to group by user in case a user has a bellow
  # score, but not visible solution.
  #
  # When it is the best score bellow_score return 0
  #
  # Example :
  # Given list of scores per users(A, B, C)
  # that looks likes A-1, A-2, B-2, B-9, C-10
  # RepositoryChallenge.bellow_score(challenge_id, 10)
  # => 2 # not 9, because the visible solution for B is 2
  def self.bellow_score(challenge_id, score)
    result = entry_aggregate(
      best_score_per_user(challenge_id),
      { "$match": { "min_score": { "$lt": score } } },
      { "$sort": { "min_score": -1 } },
      { "$limit": 1 },
    ).first
    result && result['min_score'] || 0
  end

  # Return the best score for a given user_id
  # nil when player has never played
  #
  # Example:
  # RepositoryChallenge.best_player_score(challenge_id, user_id).to_a
  # => 123
  def self.best_player_score(challenge_id, player_id)
    result = entry_aggregate(
      best_score_per_user(challenge_id),
      { "$match": { "user_id": player_id } },
      { "$limit": 1 },
    ).first
    result && result['min_score']
  end

  def self.submissions(challenge_id:, min_score:, per_page:, page:)
    entry_aggregate(
      best_score_per_user(challenge_id),
      { "$match": { "min_score": { "$gte": min_score } } },
      { "$sort": { "min_score": 1, "created_at": 1 } },
      paginate(per_page: per_page, page: page)
    )
  end

  # Count number of uniq user per challenge
  #
  # Example:
  # RepositoryChallenge.count_uniq_users(challenge_id)
  # => 1266
  def self.count_uniq_users(challenge_id)
    r = entry_aggregate(
      { "$match": { "challenge_id": challenge_id } },
      { "$group": { "_id": '$user_id' } },
      { "$count": "count_users" }
    ).first || {"count_users" => 0}
    r["count_users"]
  end

  # Return number of solution that are less than visible_score
  def self.count_remaining_solutions(challenge_id, visible_score)
    sum_lines(
      best_score_per_user(challenge_id),
      { "$match": { "min_score": { "$lt": visible_score } } },
    ) || 0
  end

  # Return number of solution that are greater or equal than visible_score
  def self.count_displayed_solutions(challenge_id, visible_score)
    sum_lines(
      best_score_per_user(challenge_id),
      { "$match": { "min_score": { "$gte": visible_score } } },
    ) || 0
  end

  def self.created_by(player_id)
    challenge_aggregate(
      { "$match": { "user_id": player_id } },
      {
        "$project": {
          "_id": 1,
          "title": 1,
          "description": 1,
        },
      }
    )
  end

  def self.group_by_challenge_with_ranking(player_id)
    [
      { "$sort": { "entries.score": 1, "entries.created_at": 1 } },
      {
        "$group": {
          "_id": {
            "challenge_id": '$_id',
            "user_id": '$entries.user_id'
          },
          "user_id": { "$first": '$entries.user_id'},
          "title": { "$first": '$title'},
          "description": { "$first": '$description'},
          "created_at": { "$first": '$created_at'},
          "count_entries": { "$first": '$count_entries'},
          "best_score": { "$first": '$best_score'},
          "best_player_score": { "$first": '$entries.score'},
          "attempts": { "$sum":  1 }
        },
      },
      { "$sort": { "best_player_score": 1, "created_at": 1 } },
      {
        "$group": {
          "_id": "$_id.challenge_id",
          "items": { "$push": "$$ROOT" },
          "count_golfers": { "$sum": 1 },
        }
      },
      { "$unwind": { "path": "$items", "includeArrayIndex": "items.position" }},
      { "$match": { "items.user_id": player_id }},
      { "$sort": { "items.created_at": -1 } },
      {
        "$project": {
          "_id": 1,
          "title": "$items.title",
          "description": "$items.description",
          "created_at": "$items.created_at",
          "count_entries": "$items.count_entries",
          "best_score": "$items.best_score",
          "best_player_score": "$items.best_player_score",
          "attempts": "$items.attempts",
          "position": { "$add": ["$items.position", 1]},
          "count_golfers": 1,
        }
      },
    ]
  end

  def self.player_best_scores(player_id)
    entry_aggregate(
      { "$match": { "user_id": player_id } },
      { "$group": {
          "_id": '$challenge_id',
          "challenge_id": {"$first": '$challenge_id'},
          "best_player_score": {"$min": "$score"},
          "last_created_at": {"$max": "$created_at"},
          "attempts": { "$sum": 1 }
        } },
      { "$sort": { "last_created_at": 1 } },
    )
  end

  def self.submissions_per_player(challenge_id, player_id)
    # Expand the entries for a specific challenge.
    #
    # Then group by:
    #   - the user id, for entries from users other than the selected player;
    #   - the entry id (a unique id), for entries from the selected player.
    #
    # This grouping ensures we get all the entries for the selected player,
    # ranked together with the best entry from all other users.
    #
    # When we rank entries, they will be offset, since we now have all the
    # entries for the selected player mixed into the best entries per user
    # for this challenge. But we can easily adjust the offset of entries
    # other than the first one.
    #
    # Since the ranking is per user and not per entry, the resulting
    # ranking position for the subsequent entries represent where this
    # entry would have been ranked was it the best entry for that player.
    #
    # In the UI, we will represent those entries using #>n, rather than
    # just #n, to indicate that this is not an actual ranking position, but
    # simply a "what-if" ranking position.
    entry_aggregate(
      { "$match": { "_id": challenge_id } },
      { "$unwind": "$entries" },
      {
        "$project": {
          "_id": {
            "$cond": [
              { "$eq": [ "$entries.user_id", player_id ] },
              "$entries._id",
              "$entries.user_id",
            ]
          },
          "entry_id": "$entries._id",
          "user_id": "$entries.user_id",
          "created_at": "$entries.created_at",
          "script": "$entries.script",
          "comments": "$entries.comments",
          "score": "$entries.score",
        }
      },
      { "$sort": { "score": 1, "created_at": 1 } },
      {
        "$group": {
          "_id": "$_id",
          "entry_id": { "$first": "$entry_id" },
          "user_id": { "$first": "$user_id" },
          "created_at": { "$first": "$created_at" },
          "script": { "$first": "$script" },
          "comments": { "$first": "$comments" },
          "score": { "$first": "$score" },
        }
      },
      { "$sort": { "score": 1, "created_at": 1 } },
      {
        "$group": {
          "_id": 1,
          "items": { "$push": "$$ROOT" },
        }
      },
      { "$unwind": { "path": "$items", "includeArrayIndex": "items.position" } },
      { "$match": { "items.user_id": player_id } },
      {
        "$project": {
          "_id": "$items.entry_id",
          "entry_id": "$items.entry_id",
          "user_id": "$items.user_id",
          "created_at": "$items.created_at",
          "script": "$items.script",
          "comments": "$items.comments",
          "min_score": "$items.score",
          "position": { "$add": ["$items.position", 1] },
        }
      }
    )
  end

  def self.count_entries(challenge_id)
    r = entry_aggregate(
      { "$match": { "challenge_id": challenge_id } },
      { "$count": "count_entries" }
    ).first || {"count_entries" => 0}
    r["count_entries"]
  end
end
