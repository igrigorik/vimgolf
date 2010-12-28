module VimGolf
  class Config
    class << self
      def path
        "#{ENV['HOME']}/.vimgolf"
      end

      def put_path
        path + "/put"
      end

      def save(conf)
        File.open(path + '/config.json', 'w') do |f|
          f.puts JSON.generate(conf)
        end
      end

      def load
        File.open(path + '/config.json', 'r') do |f|
          JSON.parse(f.read)
        end
      end
    end
  end
end
