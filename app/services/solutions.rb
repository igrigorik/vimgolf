require_relative '../repositories/repository_challenge'
require_relative './solution'

class Solutions

  PER_PAGE = 100

  def initialize(player, challenge_id)
    @player = player
    @challenge_id = challenge_id
  end
  attr_reader :challenge_id
  attr_reader :player

  def each
    solutions.each_with_index do |s, i|
      yield Solution.new(s, users, i)
    end
  end

  def users
    @users ||= User.where(:_id.in => user_ids).inject({}) {|h,u| h.merge(u.id => u)}
  end

  def user_ids
    solutions.map {|c| c[:user_id] }
  end

  def solutions
    @solutions ||= RepositoryChallenge.solutions(challenge_id: challenge_id)
  end

  def user_id
    @user_id ||= Challenge.only(:user_id).find(challenge_id)
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

end
