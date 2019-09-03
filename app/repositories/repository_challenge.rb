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
  # RepositoryChallenge.collection_aggregate({"$count": 'challenge_count'}).to_a
  # => [{"challenge_count"=>102}]
  def self.collection_aggregate(*args)
    Challenge.collection.aggregate(args.flatten)
  end

  # Sum every entries of every Challenge
  #
  # Use of $group with id => nil
  # to calculate accumulated values for all the input documents as a whole.
  #
  # $group is an Aggregation Pipeline Stages
  # see https://docs.mongodb.com/manual/reference/operator/aggregation-pipeline/
  #
  # $sum, $size, $ifNull are Aggregation Pipeline Operators
  # https://docs.mongodb.com/manual/reference/operator/aggregation/
  #
  # count_entries is a new field returned by mongodb query
  # $entries is the embeded "Challenge.entries", can be null
  #
  # Returns a Mongo::Collection::View::Aggregation
  #
  # Example :
  # RepositoryChallenge.count_entries_query.to_a
  # => [{"_id"=>nil, "count_entries"=>45000}]
  def self.count_entries_query
    collection_aggregate({
      "$group": {
        "_id": nil,
        "count_entries": {
          "$sum": {
            "$size":  { "$ifNull": [ "$entries", [] ] }
          },
        },
      },
    })
  end

  def self.count_entries
    result = count_entries_query.first

    result ? result['count_entries'] : 0
  end

  # It sort challenge by 'score' and return only the sum of entries.
  # It limit the fields returned by mongodb to get a quick response.
  # .score_query is a list of pipeline operators for .collection_aggregate
  #
  # $project:
  # - limit the returned fields
  # - add a new field 'count_entries'
  #
  # $addFields: add a new computed field 'score',
  # the equivalent ruby code of is:
  # challenge.entries.count.to_f / (Time.now.to_i - challenge.created_at.to_i)
  #
  # $sort: sort 'score' in descending order to allow pagination.
  #
  # $project, $addFields, $sort are Aggregation Pipeline Stages
  # see https://docs.mongodb.com/manual/reference/operator/aggregation-pipeline/
  #
  # $size, $ifNull, $divide, $subtract are Aggregation Pipeline Operators
  # https://docs.mongodb.com/manual/reference/operator/aggregation/
  #
  # Example:
  # RepositoryChallenge.collection_aggregate(RepositoryChallenge.score_query).to_a
  # => [
  #      {
  #        "_id"=>BSON::ObjectId('5af76222cfa4a41a195c774b'),
  #        "title"=>"Stranger in a Strange Land",
  #        "description"=>"description 39",
  #        "created_at"=>2018-05-12 21:52:33 UTC,
  #        "count_entries"=>4000,
  #        "score"=>0.044298750775228136
  #      },
  #   ....
  # ]
  def self.score_query
    [
      {
        "$project": {
          "_id": 1,
          "title": 1,
          "description": 1,
          "created_at": 1,
          "count_entries": {
            "$size":  { "$ifNull": [ "$entries", [] ] }
          }
        },
      },
      {
        "$addFields": {
          "score": {
            "$divide": [
              "$count_entries",
              { "$subtract": [Time.now, "$created_at"] }
            ]
          }
        }
      },
      { "$sort": { "score": -1 } },
    ]
  end

  # List of pipeline operators for .collection_aggregate
  # to paginate every query
  # see https://docs.mongodb.com/manual/reference/operator/aggregation/project/
  #
  # Example:
  # RepositoryChallenge.collection_aggregate(
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
    collection_aggregate(
      score_query,
      paginate(per_page: per_page, page: page)
    )
  end

  # Reference query
  def self.best_score_per_user(challenge_id)
    [
      { "$match": { "_id": challenge_id } },
      { "$unwind": "$entries" },
      # sort needed so all { "$first": key }
      # can return the right value associated to the min score
      { "$sort": { "entries.score": 1, "entries.created_at": 1 } },
      {
        '$group': {
          "_id": '$entries.user_id',
          "entry_id": { "$first": '$entries._id' },
          "user_id": { "$first": '$entries.user_id' },
          "created_at": { "$first": '$entries.created_at' },
          "script": { "$first": '$entries.script' },
          "comments": { "$first": '$entries.comments' },
          "min_score": { "$first": '$entries.score'}
        }
      },
      { "$sort": { "min_score": 1, "created_at": 1 } },
    ]
  end

  def self.best_score_for_user(challenge_id, user)
    [
      { "$match": { "_id": challenge_id } },
      { "$unwind": "$entries" },
      { "$match": { "entries.user_id": user.id } },
      { "$sort": { "entries.score": 1, "entries.created_at": 1 } },
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
    result = collection_aggregate(
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
    collection_aggregate(
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
    result = collection_aggregate(
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
    result = collection_aggregate(
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
    result = collection_aggregate(
      best_score_per_user(challenge_id),
      { "$match": { "user_id": player_id } },
      { "$limit": 1 },
    ).first
    result && result['min_score']
  end

  def self.submissions(challenge_id:, min_score:, per_page:, page:)
    collection_aggregate(
      best_score_per_user(challenge_id),
      { "$match": { "min_score": { "$gte": min_score } } },
      { "$sort": { "min_score": 1, "created_at": 1 } },
      paginate(per_page: per_page, page: page)
    )
  end

  def self.submissions_per_user(challenge_id:, min_score:, user:)
    collection_aggregate(
      best_score_for_user(challenge_id, user),
      { "$match": { "min_score": { "$gte": min_score } } },
      { "$sort": { "min_score": 1, "created_at": 1 } },
    )
  end

  # Count number of uniq user per challenge
  #
  # Example:
  # RepositoryChallenge.count_uniq_users(challenge_id)
  # => 1266
  def self.count_uniq_users(challenge_id)
    sum_lines(best_score_per_user(challenge_id)) || 0
  end

  # Load specific fields for page 'show' without loading ALL entries
  #
  # Example:
  # RepositoryChallenge.show_challenge(challenge_id).to_a
  # => [{"_id"=>BSON::ObjectId('5b1c53666e9552257783d43f'),
  # "title"=>"a title",
  # "description"=>"a description",
  # "diff"=>"diff",
  # "input"=>"input",
  # "output"=>"output",
  # "user_id"=>BSON::ObjectId('5b1c53656e9552257783c146'),
  # "count_entries"=>2000}]
  def self.show_challenge(challenge_id)
    collection_aggregate(
      { "$match": { "_id": challenge_id } },
      {
        '$project': {
          "_id": 1,
          "user_id": 1,
          "title": 1,
          "description": 1,
          "input": 1,
          "output": 1,
          "diff": 1,
          "count_entries": {
            "$size":  { "$ifNull": [ "$entries", [] ] }
          },
        }
      }
    ).first
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
    collection_aggregate(
      { "$match": { "user_id": player_id } },
      {
        "$project": {
          "_id": 1,
          "title": 1,
          "description": 1,
          "count_entries": {
            "$size":  { "$ifNull": [ "$entries", [] ] }
          }
        },
      }
    )
  end

  def self.player_best_scores(player_id)
    collection_aggregate(
      { "$match": { "entries.user_id": player_id } },
      {
        "$project": {
          "_id": 1,
          "title": 1,
          "description": 1,
          "entries": 1,
          "count_entries": {
            "$size":  { "$ifNull": [ "$entries", [] ] }
          },
          "best_score": {
            "$min": "$entries.score"
          },
        }
      },
      { "$unwind": "$entries" },
      { "$match": { "entries.user_id": player_id } },
      { "$sort": { "entries.score": 1 } },
      {
        "$group": {
          "_id": '$_id',
          "title": { "$first": '$title'},
          "description": { "$first": '$description'},
          "count_entries": { "$first": '$count_entries'},
          "best_score": { "$first": '$best_score'},
          "best_player_score": { "$first": '$entries.score'},
          "attempts": { "$sum":  1 }
        },
      }
    )
  end

end
