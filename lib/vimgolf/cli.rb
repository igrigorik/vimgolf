module VimGolf
  class Error
  end

  class UI
    def debug(*); end
  end

  class << self
    attr_accessor :ui

    def ui
      @ui ||= UI.new
    end
  end

  class CLI < Thor
    include Thor::Actions

    def self.start(*)
      Thor::Base.shell = VimGolf::CLI::UI
      VimGolf.ui = VimGolf::CLI::UI.new
      super
    end

    desc "setup", "configure your VIM Golf credentials"
    long_desc <<-DESC
    To participate in the challenge please go to vimgolf.com and register an
    account. Once signed in, you will get your API token, which you need to
    setup the command client.

    If you have the token, simply run the setup and paste in your token.
    DESC

    def setup
      token = VimGolf.ui.ask "Please specify your VIM Golf API token (register on vimgolf.com to get it):"

      if token =~ /[\w\d]{1,32}/
        FileUtils.mkdir_p Config.path
        FileUtils.mkdir_p Config.path + "/put"
        Config.save({:key => token})

        VimGolf.ui.info "Saved. Happy golfing!"
      else
        VimGolf.ui.error "Invalid token, please double check your token on vimgolf.com"
      end
    end

    desc "put [ID]", "launch VIM session"
    long_desc <<-DESC
    Launch a VimGolf session for the specified challenge ID. To find a currently
    active challenge ID, please visit vimgolf.com!
    DESC

    def put(id = nil)
      VimGolf.ui.info "Launching VimGolf session for challenge: #{id}"

      # - Z - start in restricted mode - no system commands
      # - n - no swap file, memory only editing
      # - --noplugin - don't load any plugins, lets be fair!
      # - +0 - always start on line 0
      system("vim -Z -n --noplugin +0 -W #{challenge_log(id)} #{challenge(id)}")

      if $?.exitstatus.zero?
        score = File.size(challenge_log(id))
        VimGolf.ui.info "Session recorded, your score: #{score}"

        if VimGolf.ui.yes? "Upload result to VimGolf? (yes / no)"
          VimGolf.ui.info "Uploading to VimGolf..."
          # TODO

          VimGolf.ui.info "Uploaded, thanks for golfing!"

        else
          VimGolf.ui.warn "Skipping upload. Thanks for playing. Give it another shot!"
        end

      else
        error = <<-MSG
        Uh oh, VIM did not exit properly. If the problem persists, please
        report the error on github.com/igrigorik/vimgolf
        MSG

        VimGolf.ui.error error
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