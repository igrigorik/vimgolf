module VimGolf

  GOLFDEBUG    = ENV['GOLFDEBUG'].to_sym rescue false
  GOLFHOST     = ENV['GOLFHOST']     || "http://www.vimgolf.com"
  GOLFSHOWDIFF = ENV['GOLFSHOWDIFF'] || 'vim -d -n'
  GOLFVIM      = ENV['GOLFVIM']      || 'vim'
  PROXY        = ENV['http_proxy']   || ''

  class Error
  end

  class RetryException < Exception; end

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
      VimGolf.ui.info "\nLet's setup your VimGolf key..."
      VimGolf.ui.warn "1) Open vimgolf.com in your browser."
      VimGolf.ui.warn "2) Click \"Sign in with Twitter\"."
      VimGolf.ui.warn "3) Once signed in, copy your key (black box, top right)."

      key = VimGolf.ui.ask "\nPaste your VimGolf key:"

      if key =~ /[\w\d]{32}/
        FileUtils.mkdir_p Config.path
        FileUtils.mkdir_p Config.put_path
        Config.save({'key' => key})

        VimGolf.ui.info "Saved. Happy golfing!"
      else
        VimGolf.ui.error "Invalid key, please double check your key on vimgolf.com"
      end
    end

    desc "put CHALLENGE_ID", "launch vimgolf.com challenge"
    long_desc <<-DESC
    Launch a VimGolf session for the specified challenge ID. To find a currently
    active challenge ID, please visit vimgolf.com!
    DESC

    def put(id)
      FileUtils.mkdir_p Config.put_path
      VimGolf.ui.warn "Downloading Vimgolf challenge: #{id}"
      VimGolf::Challenge.path(Config.put_path)
      challenge = Challenge.new(id)
      challenge.download

      play(challenge)
    end

    desc "local INFILE OUTFILE", "launch local challenge"
    long_desc <<-DESC
    Launch a local VimGolf challenge. A temporary copy of INFILE is made; the original files will not be touched.
    DESC

    def local(infile, outfile)
      # make sure our files are sane
      if !(File.file?(infile) and File.file?(outfile))
        VimGolf.ui.error "INFILE and OUTFILE must exist and be regular files."
        exit 1
      end

      challenge = Challenge.new(infile) # use the filename as id
      challenge.local(infile, outfile)

      play(challenge)
    end

    private

    def play(challenge)
      begin
        challenge.start
        VimGolf.ui.warn "Launching VimGolf session for challenge: #{challenge.id}"
        # -Z         - restricted mode, utilities not allowed
        # -n         - no swap file, memory only editing
        # --noplugin - don't load any plugins, lets be fair!
        # --nofork   - otherwise GOLFVIM=gvim forks and returns immediately
        # -i NONE    - don't load .viminfo (for saved macros and the like)
        # +0         - always start on line 0
        # -u vimrc   - load vimgolf .vimrc to level the playing field
        # -U NONE    - don't load .gvimrc
        # -W logfile - keylog file (overwrites if already exists)
        vimcmd = GOLFVIM.shellsplit + %W{-Z -n --noplugin --nofork -i NONE +0 -u #{challenge.vimrc_path} -U NONE -W #{challenge.log_path} #{challenge.work_path}}
        debug(vimcmd)
        system(*vimcmd) # assembled as an array, bypasses the shell

        if $?.exitstatus.zero?
          log = Keylog.new(IO.binread(challenge.log_path))

          VimGolf.ui.info "\nHere are your keystrokes:"
          VimGolf.ui.print_log log

          # Handle incorrect solutions
          if !challenge.correct?()
            VimGolf.ui.error "\nUh oh, looks like your entry does not match the desired output."
            VimGolf.ui.error "Your score for this failed attempt was: #{log.score}"
            loop do
              VimGolf.ui.warn "[d] Show diff"
              VimGolf.ui.warn "[r] Retry the current challenge"
              VimGolf.ui.warn "[q] Quit vimgolf"

              case VimGolf.ui.ask_question "Choice> ",
                                  :type      => :warn,
                                  :choices   => [:diff, :retry, :quit]
              when :diff
                VimGolf.ui.warn "Showing vimdiff of your attempt (left) and correct output (right)"
                system(*GOLFSHOWDIFF.shellsplit + [challenge.work_path, challenge.output_path])
              when :retry
                VimGolf.ui.warn "Retrying current challenge..."
                raise RetryException
              when :quit
                raise Interrupt
              end
            end
          end

          # Handle correct solutions
          VimGolf.ui.info "\nSuccess! Your output matches. Your score: #{log.score}"

          loop do
            choices = []
            begin
              Config.load # raises error if user hasn't finished setup
              choices = [:w, :x]
              VimGolf.ui.warn "[w] Upload result and retry the challenge"
              VimGolf.ui.warn "[x] Upload result and quit"
            rescue
              choices = [:setup]
              VimGolf.ui.warn "[s] Set up vimgolf.com key to submit result"
            end if challenge.remote
            VimGolf.ui.warn "[r] Do not upload result and retry the challenge"
            VimGolf.ui.warn "[q] Do not upload result and quit"

            case VimGolf.ui.ask_question "Choice> ",
                                :type    => :warn,
                                :choices => choices + [:retry, :quit]
            when :w
              next unless upload?(challenge)
              raise RetryException
            when :x
              next unless upload?(challenge)
              raise Interrupt
            when :setup
              setup
              next # we can hopefully submit this time
            when :retry
              raise RetryException
            when :quit
              raise Interrupt
            end
          end

        else
          error = <<-MSG
	  Uh oh, Vim did not exit properly.
	  Please ensure you can execute 'Vim' from the commandline.
	  If the problem persists, please report the error on github.com/igrigorik/vimgolf
          MSG

          VimGolf.ui.error error
        end

      rescue RetryException => e
        retry
      end

    rescue Interrupt
      VimGolf.ui.info "\nThanks for playing!"
    rescue RuntimeError, Exception => e
      VimGolf.ui.error "Uh oh, something went wrong! Error: #{e}"
      VimGolf.ui.error "If the error persists, please report it to github.com/igrigorik/vimgolf"
    end

    def upload?(challenge)
      VimGolf.ui.warn "Uploading to VimGolf..."

      if challenge.upload == :ok
        VimGolf.ui.warn "Uploaded entry."
        VimGolf.ui.warn "View the leaderboard: #{GOLFHOST}/challenges/#{challenge.id}"
      else
        VimGolf.ui.error "Uh oh, upload failed. You're not cheating are you? :-)"
        nil
      end
    end

    def debug(msg)
      p [caller.first, msg] if GOLFDEBUG
    end
  end

end
