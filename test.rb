require_relative './app/services/submissions'
require_relative './app/services/submissions_others'

challenge = Challenge.last
user = User.first
puts "-----------"
puts "-----------"
# pp RepositoryChallenge.worst_score(challenge.id)
# s = Submissions.new(user, challenge.id, 5)
# pp s.submissions

pp RepositoryEntry.worst_score(challenge.id)
s = SubmissionsOthers.new(user, challenge.id, 5)
pp s.submissions.to_a
pp s.visible_score
pp s.bellow_player_score
pp s.worst_score
pp s.count_uniq_users
pp s.count_remaining
pp s.count_displayed



puts "-----------"
puts "-----------"
