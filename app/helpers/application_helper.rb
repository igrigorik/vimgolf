module ApplicationHelper
  def profile_link(user)
    link_to "@#{user}", profile_path(user)
  end

  def provider_profile(user_object)
    case user_object.provider.downcase
    when 'github'
      "<a href='https://www.github.com/#{user_object.nickname}' class='twitter-link' target='_new'>@#{user_object.nickname}</a>".html_safe
    when 'twitter'
      "<a href='https://www.twitter.com/#{user_object.nickname}' class='twitter-link' target='_new'>@#{user_object.nickname}</a>".html_safe
    end
  end

  def twitter_profile(user)
    "<a href='https://www.twitter.com/#{user}' class='twitter-link' target='_new'>@#{user}</a>".html_safe
  end

  def twitter_avatar(user)
    "<img src='data:image/png;base64,#{RubyIdenticon.create_base64(user, square_size: 6, grid_size: 8, border_size: 0)}' class='user'>".html_safe
  end

  def current_url
    url_for :only_path => false
  end

  def challenge_id(challenge)
    content_for(:challenge_id, challenge.urlkey) || "Your storage is empty"
  end
end
