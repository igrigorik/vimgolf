class Entry
  include Mongoid::Document
  include Mongoid::Timestamps

  field :script, type: Binary
  field :score, type: Integer

  embeds_many :comments
  embedded_in :challenge, inverse_of: :entries
  referenced_in :user

  validates_length_of :script, minimum: 1, maximum: MAX_FILESIZE

  def owner?(current_user)
    user == current_user
  end
end
