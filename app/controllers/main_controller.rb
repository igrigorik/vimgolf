require_relative '../repositories/repository_challenge'

class MainController < ApplicationController

  def index
    challenge_count = Challenge.count
    per_page = 50

    @stats = {
      users: User.count,
      challenges: challenge_count,
      entries: RepositoryChallenge.count_entries,
    }

    @challenges = RepositoryChallenge.paginate_home_page(per_page: per_page, page: param_page)

    @paginatable_array = Kaminari
      .paginate_array([], total_count: challenge_count)
      .page(param_page)
      .per(per_page)
    # if users = Rails.cache.read(:top_users)
    #   @top_users = User.find(users).sort_by {|u| users.index(u._id) }
    # end
  end

  def feed
    @challenges = Challenge
      .limit(15)
      .order('created_at DESC')

    respond_to do |format|
      format.rss { render :layout => false }
    end
  end

  private

  def param_page
    (params['page'] || 1).to_i
  end

end
