module VimGolf
  class Session < Array
    def to_s(sep = '')
      @log.join(sep)
    end
  end

  class Keylog
    def self.parse(input)
      session = Session.new
      scan(input) {|s| session << s }
      session
    end

    def self.convert(input)
      parse(input).join('')
    end

    def self.score(input)
      keys = 0
      scan(input) {|s| keys += 1 }
      keys
    end

    def self.scan(input)
      scanner = StringScanner.new(input)
      output = ""

      until scanner.eos?
        c = scanner.get_byte
        n = c.unpack('C').first

        out_char = \
        case n

          # Special platform-independent encoding stuff
          when 0x80
            code = scanner.get_byte + scanner.get_byte

            # This list has been populated by experimentation so far,
            # because I haven't bothered looking for a more authoritative
            # source.
            case code
              when "k1"; "<F1>"
              when "k2"; "<F2>"
              when "k3"; "<F3>"
              when "k4"; "<F4>"
              when "k5"; "<F5>"
              when "k6"; "<F6>"
              when "k7"; "<F7>"
              when "k8"; "<F8>"
              when "k9"; "<F9>"
              when "k;"; "<F10>"
              when "F1"; "<F11>"
              when "F2"; "<F12>"

              when "kP"; "<PageUp>"
              when "kN"; "<PageDown>"
              when "kh"; "<Home>"
              when "@7"; "<End>"
              when "kI"; "<Insert>"
              when "kD"; "<Del>"
              when "kb"; "<BS>"

              when "ku"; "<Up>"
              when "kd"; "<Down>"
              when "kl"; "<Left>"
              when "kr"; "<Right>"
              when "#4"; "<S-Left>"
              when "%i"; "<S-Right>"

              when "kB"; "<S-Tab>"
              when "\xffX"; "<C-Space>"

              else
                #puts "Unknown Vim code: #{code.inspect}"
                '<%02x-%02x>' % code.unpack('CC')
            end

            # Control characters with special names
          when 0; "<Nul>"
          when 9; "<Tab>"
          when 10; "<NL>"
          when 13; "<CR>"
          when 27; "<Esc>"

          when 127; "<Del>"

            # Otherwise, use <C-x> format
          when 0..31; "<C-#{(n + 64).chr}>"

            # The rest of ANSI is printable
          when 32..126; c

          else
            #puts "Unexpected extended ASCII: #{'%#04x' % n}"
            '<%#04x>' % n

        end

        yield out_char
      end
    end
  end
end
