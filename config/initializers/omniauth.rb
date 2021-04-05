OmniAuth.config.allowed_request_methods = [:get, :post]
OmniAuth.config.silence_get_warning = true

Rails.application.config.middleware.use OmniAuth::Builder do
  provider(
    :twitter,
    ENV.fetch('TWITTER_OAUTH_ID', 'dev') ,
    ENV.fetch('TWITTER_OAUTH_SECRET', 'dev'),
  )
end
