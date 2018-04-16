module DatabaseHelper

  # if no 'challenges' collection, website doesn't boot
  def self.create_collections
    db = Mongoid.default_client.database
    collections = db.collections

    [:challenges, :users].map do |collection|
      collection = db.collection(collection)
      collection.create unless collections.include? collection
    end
  end

  def self.empty_collections
    db = Mongoid.default_client.database
    db.collections.map(&:delete_many)
  end

end
