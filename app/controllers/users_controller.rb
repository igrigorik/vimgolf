class UsersController < ApplicationController

  def show
    @user = User.find(:conditions => {:nickname => params[:username]}).first
    return redirect_to root_path if @user.nil?

    @contributed  = Challenge.where('user_id' => @user.id).all.to_a
    @participated = Challenge.where('entries.user_id' => @user.id)
          .only('entries.user_id', 'entries.score', :title).all.to_a

    @entries = @participated.map do |c|
      c.top_entries.select {|c| c.user_id == @user.id }
    end.flatten.compact.sort_by {|e| e.challenge.top_entries.index(e)}

    respond_to do |format|
      format.html
    end
  end

  def top
    @players = Hash.new
    @challenges = Hash.new

    # build a map of all challenges
    # build a map of ranks of each challenge for each user
    Challenge.all.each do |c|
      entries = c.top_entries
      @challenges[c.id] = entries.size

      entries.each_with_index do |e, idx|
        @players[e.user_id] ||= {}
        @players[e.user_id][c.id] = idx
      end
    end

    # compute the total score of a user
    # - not participating in a challenge is equal to highest rank
    #   in that challenge
    @scores = {}
    @challenges.keys.each do |c|
      @players.keys.each do |p|
        @scores[p] ||= 0
        @scores[p] += (@players[p][c] || @challenges[c])
      end
    end

    @top = @scores.to_a.sort_by {|a| a.last }[0,100]
    @users = User.find(@top.collect(&:first)).inject({}) {|h,u| h[u.id] = u; h}

    Rails.cache.write(:top_users, @top[0,5].map(&:first))
  end
end
