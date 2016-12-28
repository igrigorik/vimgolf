class ApplicationController < ActionController::Base
  helper_method :current_user

  private

  def current_user
    @current_user ||= User.first(:conditions => {:uid => session[:user]}) if session[:user]
    @current_user
  end

  def login
    if current_user.nil?
      redirect_to root_path
    end
  end
end
