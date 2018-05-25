# This file is copied to spec/ when you run 'rails generate rspec:install'
require 'simplecov'
SimpleCov.start 'rails'

ENV["RAILS_ENV"] ||= 'test'
require File.expand_path("../../config/environment", __FILE__)
require 'rspec/rails'
require 'capybara/poltergeist'
require 'shoulda/matchers'

# Requires supporting ruby files with custom matchers and macros, etc,
# in spec/support/ and its subdirectories.
Dir[Rails.root.join("spec/support/**/*.rb")].each {|f| require f}
Capybara.javascript_driver = :poltergeist
Capybara.server = :webrick

Shoulda::Matchers.configure do |config|
  config.integrate do |with|
    with.test_framework :rspec
    with.library :rails
  end
end

RSpec.configure do |config|
  config.mock_with :rspec
  config.infer_spec_type_from_file_location!

  config.include Mongoid::Matchers

  config.before(:each) do
    DatabaseHelper.empty_collections
    OmniAuth.config.mock_auth[:twitter] = nil
  end

  config.include FactoryGirl::Syntax::Methods

  config.before(:suite) do
    DatabaseHelper.create_collections
    FactoryGirl.find_definitions
  end
end
