class EntryValidator < ActiveModel::Validator
  def validate(entry)
    k = VimGolf::Keylog.new(entry.script).convert
    entry.errors[:entry] << "Entry cannot be empty" if k.empty?
    entry.errors[:entry] << "Entry is too large" if k.length > MAX_FILESIZE
  end
end

class Entry < ActiveRecord::Base
  has_many :comments, dependent: :destroy
  belongs_to :challenge, inverse_of: :entries
  belongs_to :user

  validates_with EntryValidator, fields: [:script]

  # Returns true if the given user sent an entry
  def self.any_owned_by?(current_user)
    where(user_id: current_user.id).count > 0 if current_user.present?
  end

  # Returns true is an entry was created by given user
  def owned_by?(current_user)
    current_user && user_id == current_user.id
  end
end
