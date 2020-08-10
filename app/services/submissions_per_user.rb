require_relative '../repositories/repository_challenge'
require_relative './solution'

class SubmissionsPerUser

  def initialize(current_user, challenge_id, player)
    # @current_user is the logged in user, while @player is the user about
    # which this per-challenge page is being shown.
    #
    # Note that this is different from Submission, in which @player refers
    # to the current logged in user.
    @current_user = current_user
    @challenge_id = challenge_id
    @player = player
  end
  attr_reader :challenge_id
  attr_reader :player

  def each
    each_unfiltered do |s|
      if visible_score && s.score >= visible_score
        yield s
      end
    end
  end

  def each_unfiltered
    # Adjust entries from the query. The position in the ranking from the first
    # entry is correct, but the other entries are not actually in the ranking
    # (only the best answer for each user is.)
    #
    # So, instead, we adjust it to show after which entry in the ranking it would
    # have been, had the user not had a better entry than that one.
    #
    # To differentiate it visually, we display it as #>n instead of #n. For
    # example, #>15 instead of #15 if your entry would come right after the one
    # in position 15.
    #
    # `each_unfiltered` is used to list all scores of the user (where the
    # Leaderboard normally is), while `each` is used to display the solutions
    # that should be visible, assuming the logged in user has an entry with
    # score low enough to allow displaying them.
    adjust = 0
    submissions.each do |s|
      position = s['position'] - adjust
      if adjust > 0
        position = ">#{position}"
      end
      yield Solution.new(s, users, position)
      adjust += 1
    end
  end

  def empty?
    submissions.none?
  end

  def users
    @users ||= {player.id => player}
  end

  def submissions
    @submissions ||= RepositoryChallenge.submissions_per_player(
      challenge_id, player.id)
  end

  def visible_score
    return @visible_score if defined?(@visible_score)

    if !@current_user
      @visible_score = nil
    elsif @current_user.admin? || creator?
      @visible_score = 0
    else
      # Player's score for this challenge, or nil if player hasn't
      # made any submissions for this one.
      @visible_score = RepositoryChallenge.best_player_score(challenge_id, @current_user.id)
    end
  end

  def user_id
    @user_id ||= Challenge.only(:user_id).find(challenge_id).user_id
  end

  def highlight_owner?
    false
  end

  def creator?
    user_id == @current_user.id
  end

  def player_can_edit?(_)
    false
  end
  alias :player_can_delete? :player_can_edit?

  def count_uniq_users
    RepositoryChallenge.count_uniq_users(challenge_id)
  end

  def count_remaining
    result = 0
    each_unfiltered do |s|
      if !(visible_score && s.score >= visible_score)
        result += 1
      end
    end
    result
  end

end
