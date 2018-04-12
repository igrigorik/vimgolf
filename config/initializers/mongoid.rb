if Rails.env.development?
  Mongoid.logger = Logger.new($stdout)
  Mongo::Logger.logger = Logger.new($stdout)
end
