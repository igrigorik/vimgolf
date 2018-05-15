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
  # RepositoryChallenge.collection_aggregate({"$count" => 'challenge_count'}).to_a
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
      "$group" => {
        "_id" => nil,
        "count_entries" => {
          "$sum" => {
            "$size" =>  { "$ifNull": [ "$entries", [] ] }
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
        "$project" => {
          "_id" => 1,
          "title" => 1,
          "description" => 1,
          "created_at" => 1,
          "count_entries" => {
            "$size" =>  { "$ifNull": [ "$entries", [] ] }
          }
        },
      },
      {
        "$addFields" => {
          "score" => {
            "$divide" => [
              "$count_entries",
              { "$subtract" => [Time.now, "$created_at"] }
            ]
          }
        }
      },
      { "$sort" => { 'score' => -1 } },
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
      { "$skip" => (per_page * (page-1))  },
      { "$limit" => per_page },
    ]
  end

  def self.paginate_home_page(per_page:, page:)
    collection_aggregate(
      score_query,
      paginate(per_page: per_page, page: page)
    )
  end

  def self.best_score_per_user(challenge_id)
    [
      { "$match" => { "_id" => challenge_id } },
      { "$unwind": "$entries" },
      { '$replaceRoot': { newRoot: "$entries" } },
      # sort needed so { "$first" => '$created_at' }
      # can return the right created_at
      { "$sort" => { "score" => 1, "created_at" => 1 } },
      {
        '$group': {
          "_id" => '$user_id',
          "entry_id" => { "$first" => '$_id' },
          "user_id" => { "$first" => '$user_id' },
          "created_at" => { "$first" => '$created_at' },
          "min_score" => { "$min" => '$score'}
        }
      },
      { "$sort" => { "min_score" => 1, "created_at" => 1 } },
    ]
  end

  def self.paginate_leaderboard(challenge_id:, per_page:, page:)
    collection_aggregate(
      best_score_per_user(challenge_id),
      paginate(per_page: per_page, page: page)
    )
  end

  def self.uniq_users(challenge_id)
    collection_aggregate(
      { "$match" => { "_id" => challenge_id } },
      { "$unwind": "$entries" },
      { '$replaceRoot': { newRoot: "$entries" } },
      {
        '$group': {
          "_id" => '$user_id',
        }
      },
    )
  end

end
