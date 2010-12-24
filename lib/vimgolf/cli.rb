require 'thor'

module VimGolf
  class Error
  end

  class CLI < Thor

    desc "setup", "configure VIM Golf credentials"
    method_options :force => :boolean, :alias => :string
    def setup
      puts "setup"
      if options.force?
      end

    end

    desc "launch", "launch VIM session"
    def launch
      puts "Launching VimGolf"
      id = 'test'

      # - Z - start in restricted mode - no system commands
      # - n - no swap file, memory only editing
      # - --noplugin - don't load any plugins, lets be fair!
      # - +0 - always start on line 0
      system("vim -Z -n --noplugin +0 -W #{challenge_log(id)} #{challenge(id)}")

      if $?.exitstatus.zero?
        puts "Session recorded, processing"

        score = File.size(challenge_log(id))
        puts "Score: #{score}"

        puts ""
      else
        puts "Uh oh, VIM did not exit properly"
      end
    end

    private

    def challenge(name)
      "tmp/#{name}"
    end

    def challenge_log(name)
      challenge(name) + ".log"
    end
  end
end
