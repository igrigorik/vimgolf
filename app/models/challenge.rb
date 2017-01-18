class Challenge
  include Mongoid::Document
  include Mongoid::Timestamps

  field :title, type: String
  field :description, type: String

  field :diff, type: String
  field :input, type: String
  field :input_type, type: String
  field :output, type: String
  field :output_type, type: String

  referenced_in :user
  embeds_many :entries

  validates_presence_of :title
  validates_presence_of :description

  validates_length_of :input, minimum: 1, maximum: MAX_FILESIZE
  validates_length_of :output, minimum: 1, maximum: MAX_FILESIZE
  validates_length_of :diff, minimum: 1, maximum: MAX_FILESIZE

  def self.count_entries
    map =     "function() { if (this.entries) { emit(this._id, this.entries.length) }}"
    reduce =  "function(k, values) { return values[0]; }"

    collection.mapreduce(map, reduce, { out: "count_entries" }).find()
  end

  def top_entries
    entries.top_by_user
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
