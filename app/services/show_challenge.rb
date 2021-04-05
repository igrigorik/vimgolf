require_relative '../repositories/repository_challenge'
require 'forwardable'

# retieve specific data for show page
# avoid loading all 'entries' to limit memory
class ShowChallenge
  extend Forwardable

  def initialize(challenge)
    @challenge = challenge
  end
  attr_reader :challenge

  def_delegators :challenge, :id
  def_delegators :challenge, :user
  def_delegators :challenge, :title
  def_delegators :challenge, :description
  def_delegators :challenge, :input
  def_delegators :challenge, :output
  def_delegators :challenge, :diff
  def_delegators :challenge, :owner?

  def count_entries
    RepositoryChallenge.count_entries(challenge.id)
  end

  def count_uniq_users
    RepositoryChallenge.count_uniq_users(challenge.id)
  end

end
