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
end
