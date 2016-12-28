class Challenge
  include Mongoid::Document
  include Mongoid::Timestamps

  field :title, :type => String
  field :description, :type => String

  field :diff, :type => String
  field :input, :type => String
  field :input_type, :type => String
  field :output, :type => String
  field :output_type, :type => String

  referenced_in :user
  embeds_many :entries

  validates_presence_of :title
  validates_presence_of :description

  validates_length_of :input, :minimum => 1, :maximum => MAX_FILESIZE
  validates_length_of :output, :minimum => 1, :maximum => MAX_FILESIZE
  validates_length_of :diff, :minimum => 1, :maximum => MAX_FILESIZE

  def self.count_entries
    map =     "function() { if (this.entries) { emit(this._id, this.entries.length) }}"
    reduce =  "function(k, values) { return values[0]; }"

    collection.mapreduce(map, reduce, { out: "count_entries" }).find()
  end

  def top_entries
    entries.sort_by {|e| [e.score, e.created_at] }.uniq {|e| e.user_id }
  end

  def participator?(current_user)
    owner?(current_user) || competitor?(current_user)
  end

  def owner?(current_user)
    current_user && ((user_id == current_user.id) || current_user.admin?)
  end

  def competitor?(current_user)
    current_user && entries.detect {|e| e.user_id == current_user.id }
  end

  def allowed_entries(current_user)
    # owner + admin can see all entries
    if owner?(current_user)
      [top_entries]

    # competitors can see all entries below them
    elsif competitor?(current_user)
      top = top_entries

      # users top submission
      user_top  = top.detect {|e| e.user_id == current_user.id }
      # index of top submission with same score
      score_top = top.detect {|e| e.score == user_top.score }

      user_index = top.index(score_top)
      solution_offset = [user_index-5, 0].max

      [top[solution_offset, top.size], solution_offset]

    # non-competitors can see bottom 20%
    else
      top = top_entries
      solution_offset = (top.size * 0.2).ceil
      [top.last(solution_offset), top.size - solution_offset]
    end
  end
end
