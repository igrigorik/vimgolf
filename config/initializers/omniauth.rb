OmniAuth.config.allowed_request_methods = [:get, :post]

Rails.application.config.middleware.use OmniAuth::Builder do
  provider(
    :twitter,
    ENV.fetch('TWITTER_OAUTH_ID', 'dev') ,
    ENV.fetch('TWITTER_OAUTH_SECRET', 'dev'),
  )

  provider(
    :github,
    ENV.fetch('GITHUB_OAUTH_ID', 'dev'),
    ENV.fetch('GITHUB_OAUTH_SECRET', 'dev')
  )
end
