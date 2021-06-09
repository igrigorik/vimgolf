module ApplicationHelper
  def profile_link(user)
    link_to "@#{user}", profile_path(user)
  end

  def twitter_profile(user)
    "<a href='https://www.twitter.com/#{user}' class='twitter-link' target='_new'>@#{user}</a>".html_safe
  end

  def twitter_avatar(user)
    "<img src='https://identicon-1132.appspot.com/#{Digest::MD5.hexdigest(user)}' class='user'>".html_safe
  end

  def current_url
    url_for :only_path => false
  end

  def challenge_id(challenge)
    content_for(:challenge_id, challenge.urlkey) || "Your storage is empty"
  end
end
