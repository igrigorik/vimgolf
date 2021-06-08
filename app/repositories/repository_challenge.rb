module RepositoryChallenge

  def self.count_entries
    Entry.count
  end

  # It sort challenge by 'score' and return only the sum of entries.
  #
  # the equivalent ruby code of is:
  # challenge.entries.count.to_f / (Time.now.to_i - challenge.created_at.to_i)
  def self.paginate_home_page(per_page:, page:)
    Challenge.order(created_at: :desc).limit(per_page).offset((page - 1) * per_page)
  end

  def self.paginate_leaderboard(challenge_id:, per_page:, page:)
    Challenge
      .find(challenge_id)
      .top_entries
      .offset((page - 1) * per_page)
      .limit(per_page)
  end

  # Return the worst score for a given challenge
  # Still need to group by user in case a user has a worst score,
  # but not visible solution.
  #
  # Return nil when no entries
  def self.worst_score(challenge_urlkey)
    Challenge.find_by_urlkey(challenge_urlkey).worst_score
  end

  # Return the next lowest score bellow a given score
  # Still need to group by user in case a user has a bellow
  # score, but not visible solution.
  #
  # When it is the best score bellow_score return 0
  #
  # Example :
  # Given list of scores per users(A, B, C)
  # that looks likes A-1, A-2, B-2, B-9, C-10
  # RepositoryChallenge.bellow_score(challenge_id, 10)
  # => 2 # not 9, because the visible solution for B is 2
  def self.bellow_score(challenge_urlkey, score)
    result = Challenge
             .find_by_urlkey(challenge_urlkey)
             .remaining_solutions(score)
             .last
    result&.score || 0
  end

  # Return the best score for a given user_id
  # nil when player has never played
  #
  # Example:
  # RepositoryChallenge.best_player_score(challenge_id, user_id).to_a
  # => 123
  def self.best_player_score(challenge_urlkey, player_id)
    Challenge
      .find_by_urlkey(challenge_urlkey)
      .best_player_score(player_id)
  end

  def self.submissions(challenge_urlkey:, min_score:, per_page:, page:)
    Challenge
      .find_by_urlkey(challenge_urlkey)
      .displayed_solutions(min_score)
      .offset((page - 1) * per_page)
      .limit(per_page)
  end

  # Count number of uniq user per challenge
  #
  # Example:
  # RepositoryChallenge.count_uniq_users(challenge_id)
  # => 1266
  def self.count_uniq_users(challenge_urlkey)
    Challenge.find_by_urlkey(challenge_urlkey).count_uniq_users
  end

  # Return number of solution that are less than visible_score
  def self.count_remaining_solutions(challenge_urlkey, visible_score)
    Challenge.find_by_urlkey(challenge_urlkey).remaining_solutions(visible_score).size
  end

  # Return number of solution that are greater or equal than visible_score
  def self.count_displayed_solutions(challenge_urlkey, visible_score)
    Challenge.find_by_urlkey(challenge_urlkey).displayed_solutions(visible_score).size
  end

  def self.created_by(player_id)
    User.find(player_id).challenges
  end

  def self.player_best_scores(player_id)
    User.find(player_id).player_best_scores
  end

end
