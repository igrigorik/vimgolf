# encoding: UTF-8
# Ruby 1.9 doesn't like our fancy string literals without this.
require "cli_helper"

describe VimGolf::Keylog do

  Dir[File.join(File.dirname(__FILE__), 'fixtures', '*')].each do |f|
    it "should parse #{File.basename(f)} logfile" do
      expect { VimGolf::Keylog.new(IO.read(f)).convert }.not_to raise_error
    end

    it "should score #{File.basename(f)} logfile" do
      expect { VimGolf::Keylog.new(IO.read(f)).convert }.not_to raise_error
    end
  end

  it "should correctly parse and count a variety of keycodes" do
    input = "\x01\x09\x1a\x1b\x1c\x1e !09\\\"'()Aa~\x7f\x80KC\x80kd\x80\xffXZZ"
    output = "<C-A><Tab><C-Z><Esc><C-\\><C-^> !09\\\"'()Aa~<C-?><k0><Down><C-@>ZZ"
    log = VimGolf::Keylog.new(input)
    expect(log.convert).to eql(output)
    expect(log.score).to eql(24)
  end

  it "should be resilient to encoding mismatches" do
    text = "Здравствуйте ¡Olé! おはよう \x80\xfdQ\x80\xfeX".force_encoding("UTF-8")
    # .force_encoding CHANGES THE ORIGINAL STRING!
    bytes = text.dup.force_encoding(Encoding::ASCII_8BIT)
     
    # Sanity check. Different rubies conspiring to mess up the test encodings.
    expect(bytes.encoding).not_to eql(text.encoding)
    # Not testing the actual output, just checking whether encoding matters
    expect(VimGolf::Keylog.new(text).convert).to eql(VimGolf::Keylog.new(bytes).convert)
  end

  it "should treat newline characters literally" do
    # Depending on your parsing strategy, could be a problem
    input = "a\ra\na\r\na\n\r\n"
    output = "a<CR>a<NL>a<CR><NL>a<NL><CR><NL>"

    log = VimGolf::Keylog.new(input)
    expect(log.convert).to eql(output)
  end

  it "should ignore certain non-keystroke keycodes" do
    input = "\x80\xfd\x35\x80\xfdbhello\x80\xfd\x61\x80\xfd\x62 world"
    output = "hello world"

    log = VimGolf::Keylog.new(input)
    expect(log.convert).to eql(output)
    expect(log.score).to eql(11)
  end

  it "should handle unexpected keycodes with a fallback" do
    input = "\x80  hello"
    output = "<20-20>hello"

    log = VimGolf::Keylog.new(input)
    expect(log.convert).to eql(output)
  end
  
  it "should parse some keycodes differently depending on date submitted" do
    early  = "\x80\xfd\x55\x80\xfd\x56\x80\xfd\x57\x80\xfd\x58\x80\xfd\x2c"
    late   = "\x80\xfd\x54\x80\xfd\x55\x80\xfd\x56\x80\xfd\x57\x80\xfd\x2c"
    output = "<C-Left><C-Right><C-Home><C-End><LeftMouse>"

    expect(VimGolf::Keylog.new(early, Time.utc(2016, 3)).convert).to eql(output)
    expect(VimGolf::Keylog.new(late, Time.utc(2016, 5)).convert).to eql(output)
  end
end
