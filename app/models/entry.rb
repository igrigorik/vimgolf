class Entry
  include Mongoid::Document
  include Mongoid::Timestamps

  field :script, :type => Binary
  field :score, :type => Integer
  field :downvotes, :type => Array, :default => []
  field :upvotes, :type => Array, :default => []

  embeds_many :comments
  embedded_in :challenge, :inverse_of => :entries
  referenced_in :user

  validates_length_of :script, :minimum => 1, :maximum => MAX_FILESIZE

  def upvote(current_user)
    migrate(:upvotes, [])
    upvotes.push current_user if !upvotes.include? current_user
  end

  def downvote(current_user)
    migrate(:downvotes, [])
    downvotes.push current_user if !downvotes.include? current_user
  end

  def voted?(current_user)
    migrate(:upvotes, [])
    migrate(:downvotes, [])

    upvotes.include?(current_user) || downvotes.include?(current_user)
  end

  def owner?(current_user)
    user == current_user
  end

  private
    def migrate(name, type)
      self[name] = type if self[name].nil?
    end
end
