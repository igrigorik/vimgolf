require_relative '../repositories/repository_challenge'

class Leaderboard

  PER_PAGE = 30

  def initialize(challenge, page)
    @challenge = challenge
    @page = (page || 1).to_i
  end
  attr_reader :page
  attr_reader :challenge

  def each
    position = PER_PAGE * (page-1) + 1

    entries.each_with_index do |entry, index|
      yield(entry, User.find(entry[:user_id]), position + index)
    end
  end

  def entries
    @entries ||= RepositoryChallenge.paginate_leaderboard(challenge_id: challenge.id, per_page: PER_PAGE, page: page)
  end

  def empty?
    entries.count.zero?
  end

  def paginated
    Kaminari
      .paginate_array([], total_count: RepositoryChallenge.count_uniq_users(challenge.id))
      .page(page)
      .per(PER_PAGE)
  end

end
