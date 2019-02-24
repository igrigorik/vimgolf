# encoding: ASCII-8BIT
# Force encoding of string literals. Must match solution text.

module VimGolf
  class Keylog
    include Enumerable

    def initialize(input, time=Time.now.utc)
      # Force encoding of solution text. Must match string literals.
      # .force_encoding CHANGES THE ORIGINAL STRING!
      @input = input.force_encoding(Encoding::ASCII_8BIT)
      @time = time
    end

    def to_s(sep = '')
      to_a.join(sep)
    end

    alias_method :convert , :to_s
    alias_method :score   , :count

    def each
      scanner = StringScanner.new(@input)

      # A Vim keycode is either a single byte, or a 3-byte sequence starting
      # with 0x80.
      while (c = scanner.get_byte)
        n = c.ord
        if n == 0x80
          b2, b3 = scanner.get_byte, scanner.get_byte
          code = KC_MBYTE[b2+b3]
          yield code if code # ignore "nil" keystrokes (like window focus)
        else
          yield KC_1BYTE[n]
        end
      end
    end

    # Quick lookup array for single-byte keycodes
    KC_1BYTE = []
    (0..255).each {|n| KC_1BYTE.push("<%#04x>" % n)} # Fallback for non-ASCII
    (1..127).each {|n| KC_1BYTE[n] = "<C-#{(n ^ 0x40).chr}>"}
    (32..126).each {|c| KC_1BYTE[c] = c.chr } # Printing chars
    KC_1BYTE[0x1b] = "<Esc>" # Special names for a few control chars
    KC_1BYTE[0x0d] = "<CR>"
    KC_1BYTE[0x0a] = "<NL>"
    KC_1BYTE[0x09] = "<Tab>"

    KC_MBYTE = Hash.new do |_h,k|
      '<' + k.bytes.map {|b| "%02x" % b}.join('-') + '>' # For missing keycodes
    end.update({
      # This list has been populated by looking at
      # :h terminal-options and vim source files:
      # keymap.h and misc2.c
      "k1" => "<F1>",
      "k2" => "<F2>",
      "k3" => "<F3>",
      "k4" => "<F4>",
      "k5" => "<F5>",
      "k6" => "<F6>",
      "k7" => "<F7>",
      "k8" => "<F8>",
      "k9" => "<F9>",
      "k;" => "<F10>",
      "F1" => "<F11>",
      "F2" => "<F12>",
      "F3" => "<F13>",
      "F4" => "<F14>",
      "F5" => "<F15>",
      "F6" => "<F16>",
      "F7" => "<F17>",
      "F8" => "<F18>",
      "F9" => "<F19>",

      "%1" => "<Help>",
      "&8" => "<Undo>",
      "#2" => "<S-Home>",
      "*7" => "<S-End>",
      "K1" => "<kHome>",
      "K4" => "<kEnd>",
      "K3" => "<kPageUp>",
      "K5" => "<kPageDown>",
      "K6" => "<kPlus>",
      "K7" => "<kMinus>",
      "K8" => "<kDivide>",
      "K9" => "<kMultiply>",
      "KA" => "<kEnter>",
      "KB" => "<kPoint>",
      "KC" => "<k0>",
      "KD" => "<k1>",
      "KE" => "<k2>",
      "KF" => "<k3>",
      "KG" => "<k4>",
      "KH" => "<k5>",
      "KI" => "<k6>",
      "KJ" => "<k7>",
      "KK" => "<k8>",
      "KL" => "<k9>",

      "kP" => "<PageUp>",
      "kN" => "<PageDown>",
      "kh" => "<Home>",
      "@7" => "<End>",
      "kI" => "<Insert>",
      "kD" => "<Del>",
      "kb" => "<BS>",

      "ku" => "<Up>",
      "kd" => "<Down>",
      "kl" => "<Left>",
      "kr" => "<Right>",
      "#4" => "<S-Left>",
      "%i" => "<S-Right>",

      "kB" => "<S-Tab>",
      "\xffX" => "<C-@>",

      # This is how you escape literal 0x80
      "\xfeX" => "<0x80>",

      # These rarely-used modifiers should be combined with the next
      # stroke (like <S-Space>), but let's put them here for now
      "\xfc\x02" => "<S->",
      "\xfc\x04" => "<C->",
      "\xfc\x06" => "<C-S->",
      "\xfc\x08" => "<A->",
      "\xfc\x0a" => "<A-S->",
      "\xfc\x0c" => "<C-A>",
      "\xfc\x0e" => "<C-A-S->",
      "\xfc\x10" => "<M->",
      "\xfc\x12" => "<M-S->",
      "\xfc\x14" => "<M-C->",
      "\xfc\x16" => "<M-C-S->",
      "\xfc\x18" => "<M-A->",
      "\xfc\x1a" => "<M-A-S->",
      "\xfc\x1c" => "<M-C-A>",
      "\xfc\x1e" => "<M-C-A-S->",

      # KS_EXTRA keycodes (starting with 0x80 0xfd) are defined by an enum in
      # Vim's keymap.h. Sometimes, a new Vim adds or removes a keycode, which
      # changes the binary representation of every keycode after it. Very
      # annoying.
      "\xfd\x4" => "<S-Up>",
      "\xfd\x5" => "<S-Down>",
      "\xfd\x6" => "<S-F1>",
      "\xfd\x7" => "<S-F2>",
      "\xfd\x8" => "<S-F3>",
      "\xfd\x9" => "<S-F4>",
      "\xfd\xa" => "<S-F5>",
      "\xfd\xb" => "<S-F6>",
      "\xfd\xc" => "<S-F7>",
      "\xfd\xd" => "<S-F9>",
      "\xfd\xe" => "<S-F10>",
      "\xfd\xf" => "<S-F10>",
      "\xfd\x10" => "<S-F11>",
      "\xfd\x11" => "<S-F12>",
      "\xfd\x12" => "<S-F13>",
      "\xfd\x13" => "<S-F14>",
      "\xfd\x14" => "<S-F15>",
      "\xfd\x15" => "<S-F16>",
      "\xfd\x16" => "<S-F17>",
      "\xfd\x17" => "<S-F18>",
      "\xfd\x18" => "<S-F19>",
      "\xfd\x19" => "<S-F20>",
      "\xfd\x1a" => "<S-F21>",
      "\xfd\x1b" => "<S-F22>",
      "\xfd\x1c" => "<S-F23>",
      "\xfd\x1d" => "<S-F24>",
      "\xfd\x1e" => "<S-F25>",
      "\xfd\x1f" => "<S-F26>",
      "\xfd\x20" => "<S-F27>",
      "\xfd\x21" => "<S-F28>",
      "\xfd\x22" => "<S-F29>",
      "\xfd\x23" => "<S-F30>",
      "\xfd\x24" => "<S-F31>",
      "\xfd\x25" => "<S-F32>",
      "\xfd\x26" => "<S-F33>",
      "\xfd\x27" => "<S-F34>",
      "\xfd\x28" => "<S-F35>",
      "\xfd\x29" => "<S-F36>",
      "\xfd\x2a" => "<S-F37>",
      "\xfd\x2b" => "<Mouse>",
      "\xfd\x2c" => "<LeftMouse>",
      "\xfd\x2d" => "<LeftDrag>",
      "\xfd\x2e" => "<LeftRelease>",
      "\xfd\x2f" => "<MiddleMouse>",
      "\xfd\x30" => "<MiddleDrag>",
      "\xfd\x31" => "<MiddleRelease>",
      "\xfd\x32" => "<RightMouse>",
      "\xfd\x33" => "<RightDrag>",
      "\xfd\x34" => "<RightRelease>",
      "\xfd\x35" => nil, # KE_IGNORE
      #"\xfd\x36" => "KE_TAB",
      #"\xfd\x37" => "KE_S_TAB_OLD",

      #"\xfd\x38" => "KE_SNIFF_UNUSED",
      #"\xfd\x39" => "KE_XF1",
      #"\xfd\x3a" => "KE_XF2",
      #"\xfd\x3b" => "KE_XF3",
      #"\xfd\x3c" => "KE_XF4",
      #"\xfd\x3d" => "KE_XEND",
      #"\xfd\x3e" => "KE_ZEND",
      #"\xfd\x3f" => "KE_XHOME",
      #"\xfd\x40" => "KE_ZHOME",
      #"\xfd\x41" => "KE_XUP",
      #"\xfd\x42" => "KE_XDOWN",
      #"\xfd\x43" => "KE_XLEFT",
      #"\xfd\x44" => "KE_XRIGHT",
      #"\xfd\x45" => "KE_LEFTMOUSE_NM",
      #"\xfd\x46" => "KE_LEFTRELEASE_NM",
      #"\xfd\x47" => "KE_S_XF1",
      #"\xfd\x48" => "KE_S_XF2",
      #"\xfd\x49" => "KE_S_XF3",
      #"\xfd\x4a" => "KE_S_XF4",
      "\xfd\x4b" => "<ScrollWheelUp>",
      "\xfd\x4c" => "<ScrollWheelDown>",

      # Horizontal scroll wheel support was added in Vim 7.3c. These
      # 2 entries shifted the rest of the KS_EXTRA mappings down 2.
      # Though Vim 7.2 is rare today, it was common soon after
      # vimgolf.com was launched. In cases where the 7.3 code is
      # never used but the 7.2 code was common, it makes sense to use
      # the 7.2 code. There are conflicts though, so some legacy
      # keycodes have to stay wrong.
      "\xfd\x4d" => "<ScrollWheelRight>",
      "\xfd\x4e" => "<ScrollWheelLeft>",
      "\xfd\x4f" => "<kInsert>",
      "\xfd\x50" => "<kDel>",
      "\xfd\x51" => "<0x9b>", # :help <CSI>
      #"\xfd\x52" => "KE_SNR",
      #"\xfd\x53" => "KE_PLUG", # never used
      "\xfd\x53" => "<C-Left>", # 7.2 compat
      #"\xfd\x54" => "KE_CMDWIN", # never used
      "\xfd\x54" => "<C-Right>", # 7.2 compat
      "\xfd\x55" => "<C-Left>", # 7.2 <C-Home> conflict
      "\xfd\x56" => "<C-Right>", # 7.2 <C-End> conflict
      "\xfd\x57" => "<C-Home>",
      "\xfd\x58" => "<C-End>",
      #"\xfd\x59" => "KE_X1MOUSE",
      #"\xfd\x5a" => "KE_X1DRAG",
      #"\xfd\x5b" => "KE_X1RELEASE",
      #"\xfd\x5c" => "KE_X2MOUSE",
      #"\xfd\x5d" => "KE_X2DRAG",
      #"\xfd\x5e" => "KE_X2RELEASE",
      "\xfd\x5e" => nil, # 7.2 compat (I think?)
      #"\xfd\x5f" => "KE_DROP",
      #"\xfd\x60" => "KE_CURSORHOLD",

      # If you use gvim, you'll get an entry in your keylog every time the
      # window gains or loses focus. These "keystrokes" should not show and
      # should not be counted.
      "\xfd\x60" => nil, # 7.2 Focus Gained compat
      "\xfd\x61" => nil, # Focus Gained (GVIM) (>7.4.1433)
      "\xfd\x62" => nil, # Focus Gained (GVIM)
      "\xfd\x63" => nil, # Focus Lost (GVIM)
    })
  end
end
