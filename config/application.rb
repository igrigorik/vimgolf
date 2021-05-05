require File.expand_path('../boot', __FILE__)

require "action_controller/railtie"
require "active_model/railtie"
require "active_record/railtie"
require "action_mailer/railtie"
require "rails/test_unit/railtie"

if defined?(Bundler)
  Bundler.require(*Rails.groups)
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

    config.assets.enabled = true
    config.assets.version = "1.0"

    config.generators do |g|
      g.orm :active_record
    end
  end
end
