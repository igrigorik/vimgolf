require 'highline'

module VimGolf
  class CLI
    class UI < Thor::Base.shell

      def initialize
          super
          @hl = HighLine.new($stdin)
      end

      def error(name, message = nil)
        begin
          orig_out, $stdout = $stdout, $stderr
          if message
            say_status name, message, :red
          elsif name
            say name, :red
          end
        ensure
          $stdout = orig_out
        end
      end

      def warn(name, message = nil)
        if message
          say_status name, message, :yellow
        elsif name
          say name, :yellow
        end
      end

      def info(name, message = nil)
        if message
          say_status name, message, :green
        elsif name
          say name, :green
        end
      end

      def debug(name, message = nil)
        return unless ENV["DEBUG"]

        if message
          message = message.inspect unless message.is_a?(String)
          say_status name, message, :blue
        elsif name
          name = name.inspect unless name.is_a?(String)
          say name, :cyan
        end
      end

      def ask_question(message, options = {})
        begin
          message = color_string(message, options[:type])
          details = Proc.new do |q|
            q.readline = !$stdin.tty?
          end
          @hl.ask(message, options[:choices] || [], &details)
        rescue EOFError
          return ''
        end
      end

      def color_string(str, type)
        @hl.color(
          str,
          case type
          when :info  then :green
          when :warn  then :yellow
          when :error then :red
          when :debug then :cyan
          else nil
          end
        )
      end

      def print_envs(apps, default_env_name = nil, simple = false)
        if simple
          envs = apps.map{ |a| a.environments }
          envs.flatten.map{|x| x.name}.uniq.each do |env|
            puts env
          end
        else
          apps.each do |app|
            puts "#{app.name} (#{app.account.name})"
            if app.environments.any?
              app.environments.each do |env|
                short_name = env.shorten_name_for(app)

                icount = env.instances_count
                iname = (icount == 1) ? "instance" : "instances"

                default_text = env.name == default_env_name ? " [default]" : ""

                puts "  #{short_name}#{default_text} (#{icount} #{iname})"
              end
            else
              puts "  (This application is not in any environments; you can make one at #{EY.config.endpoint})"
            end

            puts ""
          end
        end
      end

      def print_exception(e)
        if e.message.empty? || (e.message == e.class.to_s)
          message = nil
        else
          message = e.message
        end

        if ENV["DEBUG"]
          error(e.class, message)
          e.backtrace.each{|l| say(" "*3 + l) }
        else
          error(message || e.class.to_s)
        end
      end

      def print_help(table)
        print_table(table, :ident => 2, :truncate => true, :colwidth => 20)
      end

      def set_color(string, color, bold=false)
        ($stdout.tty? || ENV['THOR_SHELL']) ? super : string
      end

    end
  end
end
