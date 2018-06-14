class Solution

  def initialize(solution, users, position)
    @solution = solution
    @users = users
    @position = position
  end
  attr_reader :solution, :position

  def id
    solution[:entry_id]
  end

  def score
    solution[:score]
  end

  def script
    solution[:script]
  end

  def comments
    solution[:comments] || []
  end

  def created_at
    solution[:created_at]
  end

  def owner?(player)
    player && player.id === solution[:user_id]
  end

  def user
    @users[solution[:user_id]]
  end

end
