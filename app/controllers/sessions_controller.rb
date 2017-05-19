class SessionsController < ApplicationController
  def create
    user = User.where(uid: request.env['omniauth.auth']['uid']).first

    if user.nil?
      user = User.create(user_params.merge({
        provider: request.env['omniauth.auth']['provider'],
        uid: request.env['omniauth.auth']['uid']
      }).permit!)
    else
      user.update_attributes(user_params)
    end

    session[:user] = user[:uid]
    redirect_to root_url, :notice => "Signed in"
  end

  def destroy
    session[:user] = nil
    redirect_to root_url, :notice => "Signed out"
  end

  private

  def user_params
    ActionController::Parameters.new(
      request.env['omniauth.auth']
    ).required("info").permit!
  end
end
