class Comment < ActiveRecord::Base
  belongs_to :user
  belongs_to :entry, inverse_of: :comments

  validates_presence_of :comment
  validates_length_of   :comment, minimum: 1

  def nickname
    user.nickname
  end
end
