class MainController < ApplicationController

  def index
    start = Time.now
    @challenges = Challenge.only(:title, :description, :created_at)
                           .order_by(:created_at.desc)
    @entry_counts = Challenge.count_entries.to_a.inject({}) do |h,v|
      h[v['_id']] = v['value'].to_i
      h
    end

    @entry_counts.default = 0
    @challenges = @challenges.sort_by do |e|
      (@entry_counts[e['_id']] + 1).to_f / (Time.now.to_i - e['created_at'].to_i)
    end.reverse

    @stats = {
      :users => User.count,
      :challenges => @challenges.size,
      :entries => @entry_counts.values.inject(:+)
    }

    if users = Rails.cache.read(:top_users)
      @top_users = User.find(users).sort_by {|u| users.index(u._id) }
    end
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
