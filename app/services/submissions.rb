require_relative '../repositories/repository_challenge'
require_relative './solution'

class Submissions

  PER_PAGE = 100

  def initialize(player, challenge_id, page)
    @player = player
    @challenge_id = challenge_id
    @page = (page || 1).to_i
  end
  attr_reader :player
  attr_reader :challenge_id
  attr_reader :page

  def each
    position = PER_PAGE * (page-1) + count_remaining + 1

    submissions.each_with_index do |s, i|
      yield Solution.new(s, users, position + i)
    end
  end

  def users
    @users ||= User.where(:_id.in => user_ids).inject({}) {|h,u| h.merge(u.id => u)}
  end

  def user_ids
    submissions.map {|c| c[:user_id] }
  end

  def submissions
    @submissions ||= RepositoryChallenge.submissions(
      challenge_id: challenge_id,
      min_score: visible_score,
      per_page: PER_PAGE,
      page: page
    )
  end

  def visible_score
    return @visible_score if defined?(@visible_score)

    if !player
      @visible_score = worst_score
    elsif player.admin? || creator?
      @visible_score = 0
    else
      @visible_score = bellow_player_score
    end
  end

  def bellow_player_score
    score = RepositoryChallenge.best_player_score(challenge_id, player.id)
    if score
      RepositoryChallenge.bellow_score(challenge_id, score)
    else
      # worst_score in case player has never played
      worst_score
    end
  end

  def worst_score
    @worst_score ||= RepositoryChallenge.worst_score(challenge_id)
  end

  def user_id
    @user_id ||= Challenge.only(:user_id).find(challenge_id).user_id
  end

  def creator?
    user_id == player.id
  end

  def player_can_edit?(solution)
    player && (
      player.admin? || creator? || solution.owner?(player)
    )
  end
  alias :player_can_delete? :player_can_edit?

  def paginated
    Kaminari
      .paginate_array([], total_count: count_displayed)
      .page(page)
      .per(PER_PAGE)
  end

  def count_uniq_users
    RepositoryChallenge.count_uniq_users(challenge_id)
  end

  def count_remaining
    @count_remaining ||= RepositoryChallenge.count_remaining_solutions(challenge_id, visible_score)
  end

  def count_displayed
    @count_displayed ||= RepositoryChallenge.count_displayed_solutions(challenge_id, visible_score)
  end

end
