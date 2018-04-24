module RepositoryChallenge

  def self.count_entries_query
    Challenge.collection.aggregate([{
      "$group" => {
        "_id" => nil,
        "count_entries" => { "$sum" => { "$size" => "$entries" }},
      },
    }])
  end

  def self.count_entries
    result = count_entries_query.first

    result ? result['count_entries'] : 0
  end

  def self.home_page
    Challenge.collection.aggregate([
      {
        "$project" => {
          "_id" => 1,
          "title" => 1,
          "description" => 1,
          "created_at" => 1,
          "count_entries" => { "$size" => "$entries" }
        },
      },
      {
        # create a new field score which is :
        # (number of entries) / (Time.now - create_at)
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
    ])
  end

end
