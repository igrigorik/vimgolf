module VimGolf

  GOLFHOST  = ENV['GOLFHOST'] || "http://vimgolf.com"
  GOLFDEBUG = ENV['GOLFDEBUG'].to_sym rescue false
  GOLFDIFF  = ENV['GOFLDIFF'] || 'diff'
  GOLFVIM   = ENV['GOLFVIM'] || 'vim'
  PROXY     = ENV['http_proxy'] || ''

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
    account. Once signed in, you will get your API key, which you need to
    setup the command client.

      If you have the key, simply run the setup and paste in your key.
      DESC

    def setup
      key = VimGolf.ui.ask "Please specify your VimGolf API key (register on vimgolf.com to get it):"

      if key =~ /[\w\d]{32}/
        FileUtils.mkdir_p Config.path
        FileUtils.mkdir_p Config.put_path
        Config.save({'key' => key})

        VimGolf.ui.info "Saved. Happy golfing!"
      else
        VimGolf.ui.error "Invalid key, please double check your key on vimgolf.com"
      end
    end

    desc "put [ID]", "launch Vim session"
    long_desc <<-DESC
    Launch a VimGolf session for the specified challenge ID. To find a currently
    active challenge ID, please visit vimgolf.com!
    DESC

    def put(id = nil)
      VimGolf.ui.warn "Launching VimGolf session for challenge: #{id}"

      begin
        type = download(id)

        # - n - no swap file, memory only editing
        # - +0 - always start on line 0
        # - --noplugin - don't load any plugins, lets be fair!
        # -i NONE - don't load .viminfo (for saved macros and the like)
        # - u - load vimgolf .vimrc to level the playing field
        vimcmd = "#{GOLFVIM} -n --noplugin -i NONE +0 -u \"#{vimrc(id)}\" -W \"#{log(id)}\" \"#{input(id, type)}\""
        debug(vimcmd)
        system(vimcmd)

        if $?.exitstatus.zero?
          diff = `#{GOLFDIFF} \"#{input(id, type)}\" \"#{output(id)}\"`
          score = Keylog.score(IO.read(log(id)))

          if diff.size > 0
            VimGolf.ui.warn "Uh oh, looks like your entry does not match the desired output:"
            VimGolf.ui.warn "#"*50
            puts diff
            VimGolf.ui.warn "#"*50
            VimGolf.ui.warn "Please try again! Your score for this (failed) attempt was: #{score}"
            return
          end

          VimGolf.ui.info "Success! Your output matches. Your score: #{score}"

          if VimGolf.ui.yes? "Upload result to VimGolf? (yes / no)"
            VimGolf.ui.warn "Uploading to VimGolf..."

            if upload(id) == :ok
              VimGolf.ui.info "Uploaded entry, thanks for golfing!"
              VimGolf.ui.info "View the leaderboard: #{GOLFHOST}/challenges/#{id}"
            else
              VimGolf.ui.error "Uh oh, upload failed. You're not cheating are you? :-)"
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

      rescue Exception => e
        VimGolf.ui.error "Uh oh, something went wrong! Error: #{e}"
        VimGolf.ui.error "If the error persists, please report it to github.com/igrigorik/vimgolf"
      end
    end

    no_tasks do
      def download(id)
        begin
          url = URI.parse("#{GOLFHOST}/challenges/#{id}.yaml")
          req = Net::HTTP::Get.new(url.path)

          proxy_url, proxy_user, proxy_pass = get_proxy
          proxy = Net::HTTP::Proxy(proxy_url.host, proxy_url.port, proxy_user, proxy_pass)
          res = proxy.start(url.host, url.port) { |http| http.request(req) }

          data = YAML.load(res.body)

          if !data.is_a? Hash
            raise

          elsif data['client'] != Vimgolf::VERSION
            VimGolf.ui.error "Client version mismatch. Installed: #{Vimgolf::VERSION}, Required: #{data['client']}."
            VimGolf.ui.error "\t gem install vimgolf"
            raise "Bad Version"
          end

          File.open(Config.put_path + "/#{id}.#{data['in']['type']}", "w") {|f| f.puts data['in']['data']}
          File.open(Config.put_path + "/#{id}.output", "w") {|f| f.puts data['out']['data']}
          File.open(Config.put_path + "/#{id}.golfrc", "w") {|f| f.puts data['vimrc']}

          data['in']['type']

        rescue Exception => e
          debug(e)
          raise "Uh oh, couldn't download or parse challenge, please verify your challenge id & client version."
        end
      end

      def upload(id)
        begin
          url = URI.parse("#{GOLFHOST}/entry.yaml")

          proxy_url, proxy_user, proxy_pass = get_proxy
          proxy = Net::HTTP::Proxy(proxy_url.host, proxy_url.port, proxy_user, proxy_pass)

          proxy.start(url.host, url.port) do |http|
            request = Net::HTTP::Post.new(url.request_uri)
            request.set_form_data({"challenge_id" => id, "apikey" => Config.load['key'], "entry" => IO.read(log(id))})
            request["Accept"] = "text/yaml"

            res = http.request(request)
            res = YAML.load(res.body)

            raise if !res.is_a? Hash
            res['status'].to_sym

          end
        rescue Exception => e
          debug(e)
          raise "Uh oh, entry upload has failed, please check your key."
        end
      end
    end

    private
      def input(id, type);  challenge(id) + ".#{type}"; end
      def output(id); challenge(id) + ".output"; end
      def log(id);    challenge(id) + ".log";    end
      def vimrc(id);  challenge(id) + ".golfrc"; end

      def challenge(id)
        Config.put_path + "/#{id}"
      end

      def get_proxy
        begin
          proxy_url = URI.parse(PROXY)
        rescue Exception => e
          VimGolf.ui.error "Invalid proxy uri in http_proxy environment variable - will try to run with out proxy"
          proxy_url = URI.parse("");
        end

        proxy_url.port ||= 80
        proxy_user, proxy_pass = proxy_url.userinfo.split(/:/) if proxy_url.userinfo

        return proxy_url, proxy_user, proxy_pass
      end

      def debug(msg)
        p [caller.first, msg] if GOLFDEBUG
      end
  end
end