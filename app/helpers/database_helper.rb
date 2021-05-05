module DatabaseHelper
  def self.client
    @client ||= Mongoid.default_client.database
  end
end
