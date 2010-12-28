module VimGolf

  GOLFHOST = ENV['GOLFHOST'] || "http://www.vimgolf.com"

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

        # - Z - start in restricted mode - no system commands
        # - n - no swap file, memory only editing
        # - --noplugin - don't load any plugins, lets be fair!
        # - +0 - always start on line 0
        system("vim -Z -n --noplugin +0 -W #{log(id)} #{input(id)}")

        if $?.exitstatus.zero?
          diff = `diff --strip-trailing-cr #{input(id)} #{output(id)}`

          if diff.size > 0
            VimGolf.ui.warn "Uh oh, looks like your entry does not match the desired output:"
            VimGolf.ui.warn "#"*50
            puts diff
            VimGolf.ui.warn "#"*50
            VimGolf.ui.warn "Please try again!"
            return
          end

          score = File.size(log(id))
          VimGolf.ui.info "Success! Your output matches. Your score: #{score}"

          if VimGolf.ui.yes? "Upload result to VimGolf? (yes / no)"
            VimGolf.ui.info "Uploading to VimGolf..."

            if upload(id) == :ok
              VimGolf.ui.info "Uploaded entry, thanks for golfing!"
              VimGolf.ui.info "View the leader board: http://vimgolf.com/challenges/#{id}"
            else
              VimGolf.ui.error "Uh oh, upload to VimGolf failed."
            end

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
          url = URI.parse("#{GOLFHOST}/challenges/#{id}.json")
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

      def upload(id)
        begin
          url = URI.parse("#{GOLFHOST}/entry.json")
          http = Net::HTTP.new(url.host, url.port)
          res = http.start do |conn|
            key = Config.load['key']
            data = "challenge_id=#{id}&entry=#{IO.read(log(id))}&apikey=#{key}"
            head = {'Accept' => 'application/json'}

            conn.post(url.path, data, head)
          end

          JSON.parse(res.body)['status'].to_sym

        rescue Exception => e
          VimGolf.ui.error "Uh oh, entry upload has failed, please check your key."
        end
      end

      def input(id);  challenge(id) + ".input"; end
      def output(id); challenge(id) + ".output"; end
      def log(id);    challenge(id) + ".log"; end

      def challenge(id)
        Config.put_path + "/#{id}"
      end
  end
end
