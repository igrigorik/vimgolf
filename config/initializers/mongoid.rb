if Rails.env.development?
  Mongoid.logger = Logger.new($stdout)
end
