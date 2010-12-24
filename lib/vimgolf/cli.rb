require 'thor'

module VimGolf
  class CLI < Thor

    desc "download ID", "download available challenge"
    method_options :force => :boolean, :alias => :string
    def download(name)
      puts "download"
      if options.force?
      end

    end

  end
end
