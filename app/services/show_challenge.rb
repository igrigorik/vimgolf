require_relative '../repositories/repository_challenge'
require 'forwardable'

# retieve specific data for show page
# avoid loading all 'entries' to limit memory
class ShowChallenge
  extend Forwardable

  def initialize(challenge_urlkey)
    @challenge_id = challenge_urlkey
  end

  def challenge
    @challenge ||= Challenge.find_by_urlkey(@challenge_id)
  end

  def_delegators :challenge, :id
  def_delegators :challenge, :urlkey
  def_delegators :challenge, :user
  def_delegators :challenge, :title
  def_delegators :challenge, :description
  def_delegators :challenge, :input
  def_delegators :challenge, :output
  def_delegators :challenge, :diff
  def_delegators :challenge, :owner?

  def count_entries
    challenge.entries.count
  end

  def count_uniq_users
    RepositoryChallenge.count_uniq_users(@challenge_id)
  end

end
