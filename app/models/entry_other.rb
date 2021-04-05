class EntryOther
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

  def owned_by?(current_user)
    current_user && user_id == current_user.id
  end
end
