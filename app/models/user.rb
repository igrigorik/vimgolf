require 'digest/md5'

class User
  include Mongoid::Document
  include Mongoid::Timestamps

  field :provider, type: String
  field :uid, type: String

  field :nickname, type: String
  field :name, type: String
  field :location, type: String
  field :image, type: String
  field :description, type: String
  field :key, type: String

  validates_presence_of :provider
  validates_presence_of :nickname
  validates_presence_of :name
  validates_presence_of :image
  validates_numericality_of :uid

  references_many :challenges, dependent: :destroy

  before_create :create_key
  before_destroy :destroy_entries

  def admin?
    ADMINS.include? nickname.downcase
  end

  protected
    def create_key
      self.key = Digest::MD5.hexdigest(id.to_s)
    end

    def destroy_entries
      Challenge.
        all.
        flat_map(&:entries).
        select { |entry| entry.user_id == id }.
        each(&:destroy)
    end
end
