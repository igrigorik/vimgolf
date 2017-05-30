source 'https://rubygems.org'
ruby '2.1.5'

gem 'rails', '4.1.16'

gem 'mongo', '2.1.2'
gem 'bson', '3.1.1'
gem 'mongoid', '5.0.0'

gem 'json', '1.8.2'
gem 'memcachier'
gem 'dalli'
gem 'omniauth'
gem 'omniauth-twitter'
gem 'tweet-button'
gem 'newrelic_rpm'
gem 'unicorn'
gem 'rack-timeout'
gem 'rails3_serve_static_assets', git: 'https://github.com/heroku/rails3_serve_static_assets.git'

gem 'vimgolf', path: 'lib/vimgolf'
#
# Needed for the new asset pipeline
group :assets do
  # Leaving them out for now until we start using asset pipeline
  #gem 'sass-rails', "~> 3.2.3"
  #gem 'coffee-rails', "~> 3.2.1"
  #gem 'uglifier', '>= 1.0.3'

end

# jQuery is the default JavaScript library in Rails 3.1
# Leaving this out for now until we start using asset pipeline
#gem 'jquery-rails'

group :test, :development do
  gem "rspec-rails", "3.6.0"
  gem "shoulda-matchers"
  gem "database_cleaner", "1.5.1"
  gem 'simplecov', :require => false
  gem "codeclimate-test-reporter", "~> 1.0.0"
  gem "capybara"
  gem "pry-byebug"
  gem "pry-stack_explorer"
  gem 'poltergeist'
  gem 'phantomjs', :require => 'phantomjs/poltergeist'
  gem 'factory_girl', '~> 4.0'
  gem 'faker'
end

