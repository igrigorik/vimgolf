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

    desc "setup", "configure your VimGolf credentials"
    long_desc <<-DESC
    To participate in the challenge please go to vimgolf.com and register an
    account. Once signed in, you will get your API token, which you need to
    setup the command client.

      If you have the token, simply run the setup and paste in your token.
      DESC

    def setup
      token = VimGolf.ui.ask "Please specify your VimGolf API token (register on vimgolf.com to get it):"

      if token =~ /[\w\d]{1,32}/
        FileUtils.mkdir_p Config.path
        FileUtils.mkdir_p Config.put_path
        Config.save({:key => token})

        VimGolf.ui.info "Saved. Happy golfing!"
      else
        VimGolf.ui.error "Invalid token, please double check your token on vimgolf.com"
      end
    end

    desc "put [ID]", "launch Vim session"
    long_desc <<-DESC
    Launch a VimGolf session for the specified challenge ID. To find a currently
    active challenge ID, please visit vimgolf.com!
    DESC

    def put(id = nil)
      VimGolf.ui.info "Launching VimGolf session for challenge: #{id}"

      if download(id) == :ok

        # TODO:
        # 1. query vimgolf site for stated ID - download challenge + final
        # 2. ...
        # 3. diff the files, if same, then score
        # 4. upload to vimgolf.com

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
          Uh oh, Vim did not exit properly. If the problem persists, please
          report the error on github.com/igrigorik/vimgolf
          MSG

          VimGolf.ui.error error
        end
      end
    end

    private
      def download(id)
        begin
          url = URI.parse("http://localhost:3000/challenges/#{id}.json")
          req = Net::HTTP::Get.new(url.path)
          res = Net::HTTP.start(url.host, url.port) {|http| http.request(req)}
          data = JSON.parse(res.body)

          File.open(Config.put_path + "/#{id}.input", "w") {|f| f.puts data['in']}
          File.open(Config.put_path + "/#{id}.output", "w") {|f| f.puts data['out']}

          :ok
        rescue Exception => e
          VimGolf.ui.error "Uh oh, couldn't download or parse challenge, please verify your challenge id."
        end
      end

      def challenge(name)
        Config.put_path + "/#{name}.input"
      end

      def challenge_log(name)
        challenge(name) + ".log"
      end
  end
end
