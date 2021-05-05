class Challenge < ActiveRecord::Base
  belongs_to :user
  has_many :entries, dependent: :destroy

  validates_presence_of :title
  validates_presence_of :description

  validates_length_of :input, minimum: 1, maximum: MAX_FILESIZE
  validates_length_of :output, minimum: 1, maximum: MAX_FILESIZE
  validates_length_of :diff, minimum: 1, maximum: MAX_FILESIZE

  # Define a 'urlkey' that mimics MongoDB ids, for compatibility with challenge
  # URLs before the conversion from MongoDB.
  #
  # For existing challenges, use the legacy MongoDB id (imported into the
  # 'legacy_urlkey' column.)
  #
  # For new challenges, create a similar format, using the '9v0' prefix (so
  # they all sort later than the MongoDB ones), followed by hex encoding of
  # the timestamp of creation of the challenge (36-bit), then the ActiveRecord
  # id of the challenge (48-bit).
  #
  # This is in a sense similar to how MongoDB ids are generated, with the most
  # significant bits encoding the timestamp of creation of the entry.
  def urlkey
    legacy_urlkey || format('9v0%09<timestamp>x%012<id>x', timestamp: created_at.to_i, id: id)
  end

  def self.find_by_newstyle_urlkey(key)
    c = find(key[12..23].to_i(16))
    return nil if c.legacy_urlkey || c.created_at.to_i != key[3..11].to_i(16)

    c
  end
  private_class_method :find_by_newstyle_urlkey

  def self.find_by_urlkey(key)
    return nil unless key.length == 24

    if key.start_with? '9v0'
      find_by_newstyle_urlkey(key)
    else
      find_by(legacy_urlkey: key)
    end
  end

  def to_param
    urlkey
  end

  def top_entries
    Entry.from(
      entries.select(
        '*',
        'row_number() OVER (PARTITION BY user_id ORDER BY score, created_at) as user_ranked_entry'
      ),
      :entries
    )
         .where(user_ranked_entry: 1)
         .order([:score, :created_at])
  end

  def best_score
    result = top_entries.first
    result&.score
  end

  def worst_score
    result = top_entries.last
    result&.score
  end

  def best_player_score(player_id)
    result = top_entries
             .where(user_id: player_id)
             .first
    result&.score
  end

  def player_entries(player_id)
    Entry.from(
      Entry.from(
        Entry.from(
          entries.select(
            '*',
            'row_number() OVER (PARTITION BY user_id ORDER BY score, created_at) as user_ranked_entry'
          ),
          :entries
        )
          .where('user_ranked_entry = 1 OR user_id = ?', player_id),
        :entries
      )
        .select(
          '*',
          'row_number() OVER (ORDER BY score, created_at) as position'
        ),
      :entries
    )
         .select('*', 'position')
         .where(user_id: player_id)
         .order([:score, :created_at])
  end

  def displayed_solutions(visible_score)
    top_entries.where('score >= ?', visible_score)
  end

  def remaining_solutions(visible_score)
    top_entries.where('score < ?', visible_score)
  end

  def count_uniq_users
    top_entries.size
  end

  def count_entries
    entries.size
  end

  def count_entries_by(player_id)
    entries.where(user_id: player_id).size
  end

  def participator?(current_user)
    owner?(current_user) || competitor?(current_user)
  end

  def owner?(current_user)
    current_user && ((user_id == current_user.id) || current_user.admin?)
  end

  def allowed_entries(current_user)
    top = top_entries

    # owner + admin can see all entries
    return [top, 0] if owner?(current_user)

    # competitors can see all entries below them
    if competitor?(current_user)
      # users top submission
      user_top  = top.detect { |e| e.user_id == current_user.id }
      # index of top submission with same score
      index = top.index { |e| e.score == user_top.score }

      solution_offset = [index - 5, 0].max

      [top[solution_offset, top.size], solution_offset]

    # non-competitors can see bottom 20%
    else
      solution_offset = (top.size * 0.2).ceil
      [top.last(solution_offset), top.size - solution_offset]
    end
  end

  private

  def competitor?(current_user)
    current_user && entries.any_owned_by?(current_user)
  end
end
