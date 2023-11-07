module VimGolf
  class Config
    class << self
      def path
        config_dir = ENV['XDG_CONFIG_HOME'] || "#{ENV['HOME']}/.config"
        config_dir + "/vimgolf"
      end

      def put_path
        cache_dir = ENV['XDG_CACHE_HOME'] || "#{ENV['HOME']}/.cache"
        cache_dir + "/vimgolf"
      end

      def save(conf)
        File.open(path + '/config.yaml', 'w') do |f|
          YAML.dump(conf, f)
        end
      end

      def _remove_old_dir
        old_dir = "#{ENV['HOME']}/.vimgolf"
        old_put_dir = old_dir + "/put"
        Dir.each_child(old_put_dir) do |f|
          File.unlink(old_put_dir + '/' + f)
        end
        Dir.rmdir(old_put_dir)
        File.unlink(old_dir + '/config.yaml')
        Dir.rmdir(old_dir)
      end

      def load
        old_path = "#{ENV['HOME']}/.vimgolf/config.yaml"
        if File.file?(old_path) then
          File.open(old_path, 'r') do |f|
            conf = YAML.load(f)
            _remove_old_dir
            FileUtils.mkdir_p path
            FileUtils.mkdir_p put_path
            save(conf)
            return conf
          end
        else
          File.open(path + '/config.yaml', 'r') do |f|
            return YAML.load(f)
          end
        end
      end
    end
  end
end
