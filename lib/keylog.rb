module VimGolf
  class Keylog
    def self.parse(input, time=Time.now.utc)
      input = input.data if input.is_a? BSON::Binary
      VimGolf::Keylog.new(input || '', time)
    end
  end
end
