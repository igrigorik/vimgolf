require_relative '../services/show_profile'

class UsersController < ApplicationController
  def show
    user = User.where(nickname: params[:username]).first
    return redirect_to root_path unless user

    @show_profile = ShowProfile.new(user, params[:ranking])

    respond_to do |format|
      format.html
    end
  end
end
