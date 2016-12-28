module ApplicationHelper
  def profile_link(user)
    link_to "@#{user}", profile_path(user)
  end

  def twitter_profile(user)
    "<a href='https://www.twitter.com/#{user}' class='twitter-link' target='_new'>@#{user}</a>".html_safe
  end

  def twitter_avatar(user)
    "<img src='https://avatars.io/twitter/#{user}?size=large' class='user'>".html_safe
  end

  def current_url(overwrite={})
    url_for :only_path => false, :params => params.merge(overwrite)
  end

  def challenge_id(challenge)
    content_for(:challenge_id, challenge.id) || "Your storage is empty"
  end
end
