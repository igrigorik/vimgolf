# Usage:
#
# 1) To import all data from MongoDB:
#
#   $ bundle exec rake db:setup mongodb:import
#
# 2) To import specific challenges only:
#
#   $ bundle exec rake db:setup mongodb:import challenge_ids=4c1234,5a2345,5b3456
#
# Pass challenge_ids=[list] with specific challenge ids (MongoDB ids) in order
# to import those challenges only. This also imports only the users that are
# referenced from those challenges, be it as an owner, as submitter of an entry,
# or by posting comments on entries belonging to those challenges.
#
# This is useful to limit the total number of rows on postgres, to create a
# staging environment with real challenges and users, but still keeping it under
# the limit for a free tier database instance.

class MongoImporter
  def initialize
    @mongo_client = DatabaseHelper.client
    @user_by_mongoid = {}
    @user_by_nickname = {}
    @cnt_users = 0
    @cnt_challenges = 0
    @cnt_entries = 0
    @cnt_comments = 0
    @time_start = Time.now
    @time_reported = @time_start
  end

  def numfmt(cnt, total = nil)
    if total
      total = cnt if total < cnt
      format(
        '%7<cnt>d / %7<total>d (%3<ratio>d%%)',
        cnt: cnt,
        total: total,
        ratio: (100.0 * cnt / total).round
      )
    else
      format('%7<cnt>d', cnt: cnt)
    end
  end

  def time_elapsed
    e = (Time.now - @time_start).round
    sec = e % 60
    min = e / 60
    format('%3<min>dmin %02<sec>dsec', min: min, sec: sec)
  end

  def initial_report
    Rails.logger.info 'MongoImporter Migration *STARTED*!'
    Rails.logger.info "Will migrate #{@total_users} users." if @total_users
    Rails.logger.info "Will migrate #{@total_challenges} challenges."
  end

  def report(title)
    cnt_objects = @cnt_users + @cnt_challenges + @cnt_entries + @cnt_comments
    Rails.logger.info <<~EDQ
      MongoImporter: #{title}

      --------------------------------------------------
      Users..............: #{numfmt(@cnt_users, @total_users)}
      Challenges.........: #{numfmt(@cnt_challenges, @total_challenges)}
      Entries............: #{numfmt(@cnt_entries)}
      Comments...........: #{numfmt(@cnt_comments)}

      Total Objects......: #{numfmt(cnt_objects)}
      Elapsed............: #{time_elapsed}
      --------------------------------------------------
    EDQ
  end

  def progress_report
    t = Time.now
    return unless t - @time_reported >= 30

    report 'Progress Report'
    @time_reported = t
  end

  def final_report
    report 'Migration *COMPLETED*!'
  end

  def add_user(mongo_user)
    u = User.create(
      name: mongo_user['name'],
      nickname: mongo_user['nickname'],
      description: mongo_user['description'],
      location: mongo_user['location'],
      image: mongo_user['image'],
      uid: mongo_user['uid'],
      key: mongo_user['key'],
      provider: mongo_user['provider'],
      created_at: mongo_user['created_at'],
      updated_at: mongo_user['updated_at']
    )
    @user_by_mongoid[mongo_user['_id']] = u
    @user_by_nickname[mongo_user['nickname']] = u
    @cnt_users += 1
    progress_report if (@cnt_users % 100).zero?
    u
  end

  def add_user_by_nickname(nickname)
    mongo_user = @done_all_users ? nil : @mongo_client[:users].find(nickname: nickname).first
    if mongo_user
      add_user(mongo_user)
    else
      # Add a user entry to attach this comment to.
      # The entry might end up being somewhat incomplete,
      # but that's OK for now.
      u = User.create(
        name: nickname,
        nickname: nickname,
        image: 'none',
        provider: 'none',
        uid: -1,
        key: ''
      )
      @user_by_nickname[nickname] = u
      @cnt_users += 1
      u
    end
  end

  def get_user_by_mongoid(mongoid)
    @user_by_mongoid[mongoid] ||
      add_user(@mongo_client[:users].find(_id: mongoid).first)
  end

  def get_user_by_nickname(nickname)
    @user_by_nickname[nickname] ||
      add_user_by_nickname(nickname)
  end

  def add_challenge(mongo_challenge)
    Rails.logger.debug "Importing challenge #{mongo_challenge['_id']}"
    @challenge = Challenge.create(
      title: mongo_challenge['title'],
      description: mongo_challenge['description'],
      diff: mongo_challenge['diff'],
      input: mongo_challenge['input'],
      input_type: mongo_challenge['input_type'],
      output: mongo_challenge['output'],
      output_type: mongo_challenge['output_type'],
      user: get_user_by_mongoid(mongo_challenge['user_id']),
      legacy_urlkey: mongo_challenge['_id'].to_s,
      created_at: mongo_challenge['created_at'],
      updated_at: mongo_challenge['updated_at']
    )
    mongo_challenge['entries']&.each do |mongo_entry|
      add_entry(mongo_entry)
    end
    @cnt_challenges += 1
    progress_report
  end

  def unencode_bson_binary(script)
    # Only some entries are stored as BSON::Binary, the
    # others are stored as direct strings.
    if script.is_a? BSON::Binary
      script.data
    else
      script
    end
  end

  def add_entry(mongo_entry)
    @entry = @challenge.entries.create(
      user: get_user_by_mongoid(mongo_entry['user_id']),
      script: unencode_bson_binary(mongo_entry['script']),
      score: mongo_entry['score'],
      created_at: mongo_entry['created_at'],
      updated_at: mongo_entry['updated_at']
    )
    mongo_entry['comments']&.each do |mongo_comment|
      add_comment(mongo_comment)
    end
    @cnt_entries += 1
    progress_report if (@cnt_entries % 500).zero?
  end

  def add_comment(mongo_comment)
    @entry.comments.create(
      user: get_user_by_nickname(mongo_comment['nickname']),
      comment: mongo_comment['comment'],
      created_at: mongo_comment['created_at'],
      updated_at: mongo_comment['updated_at']
    )
    @cnt_comments += 1
  end

  def add_all_users
    @mongo_client[:users].find.each do |u|
      add_user(u)
    end
    @done_all_users = true
  end

  def add_all_challenges
    @total_users = @mongo_client[:users].count
    @total_challenges = @mongo_client[:challenges].count
    initial_report
    @mongo_client[:users].find.each do |u|
      add_user(u)
    end
    @mongo_client[:challenges].find.each do |c|
      add_challenge(c)
    end
    final_report
  end

  def add_challenges_by_mongoid(mongoids)
    @total_challenges = mongoids.size
    initial_report
    mongoids.each do |id|
      add_challenge(@mongo_client[:challenges].find(_id: BSON::ObjectId(id)).first)
    end
    final_report
  end
end

namespace :mongodb do
  desc "Import data from MongoDB"
  task import: :environment do
    # Change logging level to :info, to avoid logging every SQL statement.
    # Also increase Mongo driver logging level to avoid its debug information.
    Rails.logger = Logger.new($stdout)
    Rails.logger.level = 1
    DatabaseHelper.client.client.logger.level = 1

    importer = MongoImporter.new
    challenges = ENV['challenge_ids']
    if challenges
      importer.add_challenges_by_mongoid(challenges.split(','))
    else
      importer.add_all_challenges
    end
  end
end
