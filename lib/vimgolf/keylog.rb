module VimGolf
  class Keylog
    include Enumerable

    alias_method :convert , :to_s
    alias_method :score   , :count

    def initialize(input)
      @input = input
    end

    def to_s(sep = '')
      to_a.join(sep)
    end

    def each
      scanner = StringScanner.new(@input)
      output = ""

      until scanner.eos?
        c = scanner.get_byte
        n = c.unpack('C').first

        out_char = \
        case n

          # Special platform-independent encoding stuff
          when 0x80
            code = scanner.get_byte + scanner.get_byte

            # This list has been populated by looking at
            # :h terminal-options and vim source files:
            # keymap.h and misc2.c
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
              when "F3"; "<F13>"
              when "F4"; "<F14>"
              when "F5"; "<F15>"
              when "F6"; "<F16>"
              when "F7"; "<F17>"
              when "F8"; "<F18>"
              when "F9"; "<F19>"

              when "%1"; "<Help>"
              when "&8"; "<Undo>"
              when "#2"; "<S-Home>"
              when "*7"; "<S-End>"
              when "K1"; "<kHome>"
              when "K4"; "<kEnd>"
              when "K3"; "<kPageUp>"
              when "K5"; "<kPageDown>"
              when "K6"; "<kPlus>"
              when "K7"; "<kMinus>"
              when "K8"; "<kDivide>"
              when "K9"; "<kMultiply>"
              when "KA"; "<kEnter>"
              when "KB"; "<kPoint>"
              when "KC"; "<k0>"
              when "KD"; "<k1>"
              when "KE"; "<k2>"
              when "KF"; "<k3>"
              when "KG"; "<k4>"
              when "KH"; "<k5>"
              when "KI"; "<k6>"
              when "KJ"; "<k7>"
              when "KK"; "<k8>"
              when "KL"; "<k9>"

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
              when "\xffX"; "<C-@>"

              when "\xfd\x4"; "<S-Up>"
              when "\xfd\x5"; "<S-Down>"
              when "\xfd\x6"; "<S-F1>"
              when "\xfd\x7"; "<S-F2>"
              when "\xfd\x8"; "<S-F3>"
              when "\xfd\x9"; "<S-F4>"
              when "\xfd\xa"; "<S-F5>"
              when "\xfd\xb"; "<S-F6>"
              when "\xfd\xc"; "<S-F7>"
              when "\xfd\xd"; "<S-F9>"
              when "\xfd\xe"; "<S-F10>"
              when "\xfd\xf"; "<S-F10>"
              when "\xfd\x10"; "<S-F11>"
              when "\xfd\x11"; "<S-F12>"
              when "\xfd\x12"; "<S-F13>"
              when "\xfd\x13"; "<S-F14>"
              when "\xfd\x14"; "<S-F15>"
              when "\xfd\x15"; "<S-F16>"
              when "\xfd\x16"; "<S-F17>"
              when "\xfd\x17"; "<S-F18>"
              when "\xfd\x18"; "<S-F19>"
              when "\xfd\x19"; "<S-F20>"
              when "\xfd\x1a"; "<S-F21>"
              when "\xfd\x1b"; "<S-F22>"
              when "\xfd\x1c"; "<S-F23>"
              when "\xfd\x1d"; "<S-F24>"
              when "\xfd\x1e"; "<S-F25>"
              when "\xfd\x1f"; "<S-F26>"
              when "\xfd\x20"; "<S-F27>"
              when "\xfd\x21"; "<S-F28>"
              when "\xfd\x22"; "<S-F29>"
              when "\xfd\x23"; "<S-F30>"
              when "\xfd\x24"; "<S-F31>"
              when "\xfd\x25"; "<S-F32>"
              when "\xfd\x26"; "<S-F33>"
              when "\xfd\x27"; "<S-F34>"
              when "\xfd\x28"; "<S-F35>"
              when "\xfd\x29"; "<S-F36>"
              when "\xfd\x2a"; "<S-F37>"
              when "\xfd\x2b"; "<Mouse>"
              when "\xfd\x2c"; "<LeftMouse>"
              when "\xfd\x2d"; "<LeftDrag>"
              when "\xfd\x2e"; "<LeftRelease>"
              when "\xfd\x2f"; "<MiddleMouse>"
              when "\xfd\x30"; "<MiddleDrag>"
              when "\xfd\x31"; "<MiddleRelease>"
              when "\xfd\x32"; "<RightMouse>"
              when "\xfd\x33"; "<RightDrag>"
              when "\xfd\x34"; "<RightRelease>"
              #when "\xfd\x35"; "KE_IGNORE"
              #when "\xfd\x36"; "KE_TAB"
              #when "\xfd\x37"; "KE_S_TAB_OLD"
              #when "\xfd\x38"; "KE_SNIFF"
              #when "\xfd\x39"; "KE_XF1"
              #when "\xfd\x3a"; "KE_XF2"
              #when "\xfd\x3b"; "KE_XF3"
              #when "\xfd\x3c"; "KE_XF4"
              #when "\xfd\x3d"; "KE_XEND"
              #when "\xfd\x3e"; "KE_ZEND"
              #when "\xfd\x3f"; "KE_XHOME"
              #when "\xfd\x40"; "KE_ZHOME"
              #when "\xfd\x41"; "KE_XUP"
              #when "\xfd\x42"; "KE_XDOWN"
              #when "\xfd\x43"; "KE_XLEFT"
              #when "\xfd\x44"; "KE_XRIGHT"
              #when "\xfd\x45"; "KE_LEFTMOUSE_NM"
              #when "\xfd\x46"; "KE_LEFTRELEASE_NM"
              #when "\xfd\x47"; "KE_S_XF1"
              #when "\xfd\x48"; "KE_S_XF2"
              #when "\xfd\x49"; "KE_S_XF3"
              #when "\xfd\x4a"; "KE_S_XF4"
              when "\xfd\x4b"; "<MouseDown>"
              when "\xfd\x4c"; "<MouseUp>"
              when "\xfd\x4d"; "<MouseLeft>"
              when "\xfd\x4e"; "<MouseRight>"
              #when "\xfd\x4f"; "KE_KINS"
              #when "\xfd\x50"; "KE_KDEL"
              #when "\xfd\x51"; "KE_CSI"
              #when "\xfd\x52"; "KE_SNR"
              #when "\xfd\x53"; "KE_PLUG"
              #when "\xfd\x54"; "KE_CMDWIN"
              when "\xfd\x55"; "<C-Left>"
              when "\xfd\x56"; "<C-Right>"
              when "\xfd\x57"; "<C-Home>"
              when "\xfd\x58"; "<C-End>"
              #when "\xfd\x59"; "KE_X1MOUSE"
              #when "\xfd\x5a"; "KE_X1DRAG"
              #when "\xfd\x5b"; "KE_X1RELEASE"
              #when "\xfd\x5c"; "KE_X2MOUSE"
              #when "\xfd\x5d"; "KE_X2DRAG"
              #when "\xfd\x5e"; "KE_X2RELEASE"
              #when "\xfd\x5f"; "KE_DROP"
              #when "\xfd\x5e"; "KE_CURSORHOLD"
              #when "\xfd\x61"; "KE_NOP"
              when "\xfd\x62"; nil # Focus Gained (GVIM)
              when "\xfd\x63"; nil # Focus Lost (GVIM)

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

        yield out_char if out_char
      end
    end
  end
end
