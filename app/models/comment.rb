class Comment
  include Mongoid::Document
  field :nickname, type: String
  field :comment, type: String

  embedded_in :entry, inverse_of: :comments

  validates_presence_of :nickname
  validates_presence_of :comment
  validates_length_of   :comment, minimum: 1
end
