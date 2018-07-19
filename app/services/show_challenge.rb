require_relative '../repositories/repository_challenge'
require 'forwardable'

# retieve specific data for show page
# avoid loading all 'entries' to limit memory
class ShowChallenge
  extend Forwardable

  def initialize(challenge_id)
    @challenge_id = challenge_id
  end

  def raw_challenge
    @raw_challenge ||= RepositoryChallenge.show_challenge(@challenge_id)
  end

  def challenge
    @challenge ||= Challenge.new(raw_challenge.reject { |k, _v| k == 'count_entries' })
  end

  def_delegators :challenge, :id
  def_delegators :challenge, :user
  def_delegators :challenge, :title
  def_delegators :challenge, :description
  def_delegators :challenge, :input
  def_delegators :challenge, :output
  def_delegators :challenge, :diff
  def_delegators :challenge, :owner?

  def count_entries
    raw_challenge[:count_entries]
  end

  def count_uniq_users
    RepositoryChallenge.count_uniq_users(@challenge_id)
  end

end
