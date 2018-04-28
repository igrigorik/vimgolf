module DatabaseHelper

  def self.client
    @client ||= Mongoid.default_client.database
  end

  # if no 'challenges' collection, website doesn't boot
  def self.create_collections
    collections = client.collections

    [:challenges, :users].map do |collection|
      collection = client.collection(collection)
      collection.create unless collections.include? collection
    end
  end

  def self.empty_collections
    client.collections.map(&:delete_many)
  end

end
