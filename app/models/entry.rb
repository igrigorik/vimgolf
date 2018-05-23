class Entry
  include Mongoid::Document
  include Mongoid::Timestamps

  field :script, type: BSON::Binary
  field :score, type: Integer

  embeds_many :comments
  embedded_in :challenge, inverse_of: :entries
  belongs_to :user

  # Returns true if the given user sent an entry
  def self.any_owned_by?(current_user)
    criteria.where(user_id: current_user.id).count > 0 if current_user.present?
  end

  # Returns best score entry per user
  def self.top_by_user
    criteria.sort_by { |e| [e.score, e.created_at] }.uniq(&:user_id)
  end

  # Returns true is an entry was created by given user
  def owned_by?(current_user)
    user == current_user
  end
end
