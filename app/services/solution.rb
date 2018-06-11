class Solution

  def initialize(solution, users, index)
    @solution = solution
    @users = users
    @index = index
  end
  attr_reader :solution, :index

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

  def offset
    index + 1
  end

  def user
    @users[solution[:user_id]]
  end

end
