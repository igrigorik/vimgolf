require 'securerandom'

class User < ActiveRecord::Base
  validates_presence_of :provider
  validates_presence_of :nickname
  validates_presence_of :name
  validates_presence_of :image
  validates_numericality_of :uid

  has_many :challenges, dependent: :destroy
  has_many :entries, dependent: :destroy

  attribute :key, :string, default: -> { SecureRandom.hex }

  def admin?
    ADMINS.include? nickname.downcase
  end

  def player_best_scores
    Entry.from(
      Entry.where(user_id: id).select(
        '*',
        'row_number() OVER (PARTITION BY challenge_id ORDER BY score, created_at) as challenge_ranked_entry'
      ),
      :entries
    )
         .where(challenge_ranked_entry: 1)
         .order('challenge_id DESC')
  end
end
