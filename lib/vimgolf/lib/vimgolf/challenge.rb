module VimGolf
  class Challenge
    attr_reader :id, :type, :otype, :remote

    def self.path(path)
      @@path = path if path
      @@path
    end

    def initialize(id)
      @id = id
    end

    def local(infile, outfile)
      @remote = false
      @input_path = File.expand_path(infile)
      @output_path = File.expand_path(outfile)
      @type = File.basename(@input_path) # extension? use the whole thing
      @otype = File.basename(@output_path)

      work_files
    end

    def work_files
      @vimrc_path = File.expand_path('../vimgolf.vimrc', __FILE__)

      # keep these Tempfile's around so they don't unlink
      @work = Tempfile.new(['vimgolf', ".#{@type}"])
      @log = Tempfile.new('golflog')
      # close tmp files, but don't unlink
      @work.close
      @log.close

      @work_path = @work.path()
      @log_path = @log.path()
    end

    def download
      @remote = true
      begin
        url = URI("#{GOLFHOST}/challenges/#{@id}.json")
        res = Net::HTTP.start(
          url.hostname,
          url.port,
          use_ssl: url.scheme == 'https'
        ) do |http|
          http.request_get(url)
        end

        @data = JSON.parse(res.body)

        if !@data.is_a? Hash
          raise

        elsif @data['client'] != Vimgolf::VERSION
          VimGolf.ui.error "Client version mismatch. Installed: #{Vimgolf::VERSION}, Required: #{@data['client']}."
          VimGolf.ui.error "\t gem install vimgolf"
          raise "Bad Version"
        end

        @data['in']['data'].gsub!(/\r\n/, "\n")
        @data['out']['data'].gsub!(/\r\n/, "\n")

        # be sure to sanitize the types
        @type = @data['in']['type'].gsub(/[^\w-]/, '.')
        @otype = @data['out']['type'].gsub(/[^\w-]/, '.')
        @input_path = path + ".input.#{@type}"
        @output_path = path + ".output.#{@otype}"

        save
        work_files
      rescue Exception => e
        debug(e)
        raise "Uh oh, couldn't download or parse challenge, please verify your challenge id & client version."
      end
    end

    def start
      FileUtils.cp(@input_path, @work_path)
    end

    def save
      File.open(input_path, "w")  {|f| f.puts @data['in']['data']}
      File.open(output_path, "w") {|f| f.puts @data['out']['data']}
    end

    def upload
      begin
        url = URI("#{GOLFHOST}/entry.json")
        res = Net::HTTP.start(
          url.hostname,
          url.port,
          use_ssl: url.scheme == 'https'
        ) do |http|
          http.request_post(
            url,
            URI.encode_www_form(
              "challenge_id" => @id,
              "apikey" => Config.load['key'],
              "entry" => IO.binread(log_path)
            ),
            "Accept" => "application/json"
          )
        end

        res = JSON.parse(res.body)

        raise if !res.is_a? Hash
        res['status'].to_sym
      rescue Exception => e
        debug(e)
        raise "Uh oh, entry upload has failed, please check your key."
      end
    end

    attr_reader :input_path
    attr_reader :work_path
    attr_reader :output_path
    attr_reader :log_path
    attr_reader :vimrc_path

    def correct?
      FileUtils.compare_file(@work_path, @output_path)
    end

    def path
      @@path + "/#{@id}"
    end

    private
      def debug(msg)
        p [caller.first, msg] if GOLFDEBUG
      end

  end
end
