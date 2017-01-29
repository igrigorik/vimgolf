require File.expand_path('../boot', __FILE__)

require "action_controller/railtie"
require "active_resource/railtie"
require "action_mailer/railtie"
require "rails/test_unit/railtie"

if defined?(Bundler)
  # If you precompile assets before deploying to production, use this line
  Bundler.require *Rails.groups(assets: %w(development test))
  # If you want your assets lazily compiled in production, use this line
  #Bundler.require(:default, :assets, Rails.env)
end

module Vimgolf
  class Application < Rails::Application
    # Settings in config/environments/* take precedence over those specified here.
    # Application configuration should go into files in config/initializers
    # -- all .rb files in that directory are automatically loaded.

    # Custom directories with classes and modules you want to be autoloadable.
    # config.autoload_paths += %W(#{config.root}/extras)

    # Only load the plugins named here, in the order given (default is alphabetical).
    # :all can be used as a placeholder for all plugins not explicitly named.
    # config.plugins = [ :exception_notification, :ssl_requirement, :all ]

    # Activate observers that should always be running.
    # config.active_record.observers = :cacher, :garbage_collector, :forum_observer

    # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
    # Run "rake -D time" for a list of tasks for finding time zone names. Default is UTC.
    # config.time_zone = 'Central Time (US & Canada)'

    # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
    # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}').to_s]
    # config.i18n.default_locale = :de

    # JavaScript files you want as :defaults (application.js is always included).
    config.action_view.javascript_expansions[:defaults] = %w()

    # Configure the default encoding used in templates for Ruby 1.9.
    config.encoding = "utf-8"

    # Configure sensitive parameters which will be filtered from the log file.
    config.filter_parameters += [:password]

    # Enable heroku logger
    config.action_controller.logger = Logger.new(STDOUT)

    # config.middleware.use "::ExceptionNotifier" , :email_prefix => "[vimgolf] ",
                               # :sender_address => %{"vimgolf" <exception@vimgolf.com>},
                               # :exception_recipients => %w{ilya@igvita.com}

    config.cache_store = :dalli_store,
                    (ENV["MEMCACHIER_SERVERS"] || "").split(","),
                    {
                      :username => ENV["MEMCACHIER_USERNAME"],
                      :password => ENV["MEMCACHIER_PASSWORD"],
                      :failover => true,
                      :socket_timeout => 1.5,
                      :socket_failure_delay => 0.2
                    }

    # Disable the asset pipeline for now
    config.assets.enabled = false
    config.assets.version = "1.0"
  end
end

# http://stackoverflow.com/questions/17754425/how-to-run-ruby-on-rails-3-with-ruby-2-0
ActionController::Base.config.relative_url_root = ''
