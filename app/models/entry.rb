class EntryValidator < ActiveModel::Validator
  def validate(entry)
    k = VimGolf::Keylog.parse(entry.script).convert
    entry.errors[:entry] << "Entry cannot be empty" if k.empty?
    entry.errors[:entry] << "Entry is too large" if k.length > MAX_FILESIZE
  end
end

class Entry
  include Mongoid::Document
  include Mongoid::Timestamps

  field :script, type: BSON::Binary
  field :score, type: Integer

  embeds_many :comments

  belongs_to :challenge
  belongs_to :user

  index "user_id" => 1
  index "challenge_id" => 1
  index({ "score": 1, "created_at": 1 })

  validates_with EntryValidator, fields: [:script]

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
    current_user && user_id == current_user.id
  end
end
