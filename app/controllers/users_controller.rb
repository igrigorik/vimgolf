require_relative '../services/show_profile'

class UsersController < ApplicationController
  def show
    user = User.find_by(id: params[:id], nickname: params[:username])
    return redirect_to root_path unless user

    @show_profile = ShowProfile.new(user)

    respond_to do |format|
      format.html
    end
  end
end
