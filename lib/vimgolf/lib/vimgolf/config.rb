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
        File.open(path + '/config.yaml', 'w') do |f|
          YAML.dump(conf, f)
        end
      end

      def load
        File.open(path + '/config.yaml', 'r') do |f|
          YAML.load(f)
        end
      end
    end
  end
end
