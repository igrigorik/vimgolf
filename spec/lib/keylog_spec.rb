require "cli_helper"

include VimGolf

describe VimGolf::Keylog do

  Dir[File.join(File.dirname(__FILE__), 'fixtures', '*')].each do |f|
    it "should parse #{File.basename(f)} logfile" do
      expect { Keylog.new(IO.read(f)).convert }.not_to raise_error
    end

    it "should score #{File.basename(f)} logfile" do
      expect { Keylog.new(IO.read(f)).convert }.not_to raise_error
    end
  end

  it "should correctly parse and count a variety of keycodes" do
    input = "\x01\x09\x1a\x1b\x1c\x1e !09\\\"'()Aa~\x7f\x80KC\x80kd\x80\xffXZZ"
    output = "<C-A><Tab><C-Z><Esc><C-\\><C-^> !09\\\"'()Aa~<C-?><k0><Down><C-@>ZZ"
    log = Keylog.new(input)
    expect(log.convert).to eql(output)
    expect(log.score).to eql(24)
  end

  it "should be resilient to encoding mismatches" do
    text = "Здравствуйте ¡Olé! おはよう \x80\xfdQ\x80\xfeX".force_encoding("UTF-8")
    bytes = text.b

    # Not testing the actual output, just checking whether encoding matters
    expect(Keylog.new(text).convert).to eql(Keylog.new(bytes).convert)
  end

  it "should treat newline characters literally" do
    # Depending on your parsing strategy, could be a problem
    input = "a\ra\na\r\na\n\r\n"
    output = "a<CR>a<NL>a<CR><NL>a<NL><CR><NL>"

    log = Keylog.new(input)
    expect(log.convert).to eql(output)
  end

  it "should ignore certain non-keystroke keycodes" do
    input = "\x80\xfd5\x80\xfdbhello\x80\xfdc world"
    output = "hello world"

    log = Keylog.new(input)
    expect(log.convert).to eql(output)
    expect(log.score).to eql(11)
  end

  it "should handle unexpected keycodes with a fallback" do
    input = "\x80  hello"
    output = "<20-20>hello"

    log = Keylog.new(input)
    expect(log.convert).to eql(output)
  end
end
