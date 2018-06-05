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
      .paginate_array([], total_count: count_uniq_user)
      .page(page)
      .per(PER_PAGE)
  end

  def count_uniq_user
    Challenge.where(id: challenge.id).distinct('entries.user_id').count
  end

end
