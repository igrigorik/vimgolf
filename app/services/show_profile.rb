require_relative '../repositories/repository_challenge'
require 'forwardable'

# retieve specific data for show page
# avoid loading all 'entries' to limit memory
class ShowProfile
  extend Forwardable

  def initialize(player)
    @player = player
  end
  attr_reader :player

  def_delegators :player, :nickname
  def_delegators :player, :name
  def_delegators :player, :description
  def_delegators :player, :location

  def contributed
    @contributed ||= RepositoryChallenge.created_by(player.id).to_a
  end

  def tried_challenges
    @tried_challenges ||= RepositoryChallenge.player_best_scores(player.id).to_a
  end

end
