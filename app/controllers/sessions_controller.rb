class SessionsController < ApplicationController
  def create
    user = User.where(uid: request.env['omniauth.auth']['uid']).first

    if user.nil?
      user = User.create(request.env['omniauth.auth']['info'].merge({
        :provider => request.env['omniauth.auth']['provider'],
        :uid => request.env['omniauth.auth']['uid']
      }))
    else
      user.update_attributes(request.env['omniauth.auth']['info'])
    end

    session[:user] = user[:uid]
    redirect_to root_url, :notice => "Signed in"
  end

  def destroy
    session[:user] = nil
    redirect_to root_url, :notice => "Signed out"
  end
end
