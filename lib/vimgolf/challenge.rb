module VimGolf
  class Challenge
    attr_reader :id, :type

    def self.path(path)
      @@path = path if path
      @@path
    end

    def initialize(id)
      @id = id
    end

    def download
      begin
        url = URI.parse("#{GOLFHOST}/challenges/#{@id}.json")
        req = Net::HTTP::Get.new(url.path)

        proxy_url, proxy_user, proxy_pass = get_proxy
        proxy = Net::HTTP::Proxy(proxy_url.host, proxy_url.port, proxy_user, proxy_pass)
        res = proxy.start(url.host, url.port) { |http| http.request(req) }

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

        @type = @data['in']['type']
        save
        start
      rescue Exception => e
        debug(e)
        raise "Uh oh, couldn't download or parse challenge, please verify your challenge id & client version."
      end
    end

    def start
      File.open(work_path, "w")  {|f| f.puts @data['in']['data']}
    end

    def save
      File.open(input_path, "w")  {|f| f.puts @data['in']['data']}
      File.open(output_path, "w") {|f| f.puts @data['out']['data']}
      File.open(vimrc_path, "w")  {|f| f.puts @data['vimrc']}
    end

    def upload
      begin
        url = URI.parse("#{GOLFHOST}/entry.yaml")

        proxy_url, proxy_user, proxy_pass = get_proxy
        proxy = Net::HTTP::Proxy(proxy_url.host, proxy_url.port, proxy_user, proxy_pass)

        proxy.start(url.host, url.port) do |http|
          request = Net::HTTP::Post.new(url.request_uri)
          request.set_form_data({"challenge_id" => @id, "apikey" => Config.load['key'], "entry" => IO.read(log_path)})
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

    def input_path;  path + ".#{@type}"; end
    def work_path;   path + ".work.#{@type}"; end
    def output_path; path + ".output"; end
    def log_path;    path + ".log";    end
    def vimrc_path;  path + ".golfrc"; end

    def path
      @@path + "/#{@id}"
    end

    private
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
