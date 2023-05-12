OmniAuth.config.allowed_request_methods = [:post]

Rails.application.config.middleware.use OmniAuth::Builder do
  provider(
    :twitter2,
    ENV.fetch('TWITTER_OAUTH_ID', 'dev') ,
    ENV.fetch('TWITTER_OAUTH_SECRET', 'dev'),
    callback_path: '/auth/twitter2/callback',
    scope: "tweet.read users.read"
  )

  provider(
    :github,
    ENV.fetch('GITHUB_OAUTH_ID', 'dev'),
    ENV.fetch('GITHUB_OAUTH_SECRET', 'dev')
  )
end