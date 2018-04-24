require_relative '../repositories/repository_challenge'

class MainController < ApplicationController

  def index
    @stats = {
      users: User.count,
      challenges: Challenge.count,
      entries: RepositoryChallenge.count_entries,
    }

    @challenges = RepositoryChallenge.home_page

    # if users = Rails.cache.read(:top_users)
    #   @top_users = User.find(users).sort_by {|u| users.index(u._id) }
    # end
  end

  def feed
    @challenges = Challenge.only(:title, :description, :created_at)
                           .limit(15)
                           .order_by(:created_at.desc)

    respond_to do |format|
      format.rss { render :layout => false }
    end
  end

end
