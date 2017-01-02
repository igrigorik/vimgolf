source 'http://rubygems.org'
ruby '2.1.5'

gem 'rails', '3.0.19'
gem 'mongo', '1.1.5'
gem 'bson', '1.1.5'
gem 'bson_ext', '1.1.5'
gem 'mongoid', '2.0.0.beta.20'
gem 'json', '1.8.2'
gem 'memcachier'
gem 'dalli'
gem 'omniauth'
gem 'omniauth-twitter'
gem 'tweet-button'
gem 'newrelic_rpm'
gem 'unicorn'
gem 'rack-timeout'
gem 'rails3_serve_static_assets', github: 'heroku/rails3_serve_static_assets'
gem 'test-unit'

gem 'vimgolf', path: 'lib/vimgolf'

group :test, :development do
	%w[rspec rspec-core rspec-expectations rspec-mocks rspec-support rspec-rails].each do |lib|
	  gem lib, git: "git://github.com/rspec/#{lib}.git"
	end
  gem "shoulda-matchers"
  gem "database_cleaner", "1.5.1"
  gem 'simplecov', :require => false
end
