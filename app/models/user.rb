require 'digest/md5'

class User
  include Mongoid::Document
  include Mongoid::Timestamps

  field :provider, :type => String
  field :uid, :type => String

  field :nickname, :type => String
  field :name, :type => String
  field :location, :type => String
  field :image, :type => String
  field :description, :type => String
  field :key, :type => String

  validates_presence_of :provider
  validates_presence_of :nickname
  validates_presence_of :name
  validates_presence_of :image
  validates_numericality_of :uid

  references_many :challenges, :dependent => :destroy
  references_many :entries, :dependent => :destroy

  before_create :create_key

  def admin?
    ADMINS.include? self.nickname.downcase
  end

  protected
    def create_key
      self.key = Digest::MD5.hexdigest(id.to_s)
    end
end
