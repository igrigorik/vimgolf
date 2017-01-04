require "spec_helper"

include VimGolf

describe VimGolf::Keylog do

  Dir['spec/fixtures/*'].each do |f|
    it "should parse #{f} logfile" do
      expect { Keylog.new(IO.read(f)).convert }.not_to raise_error
    end

    it "should score #{f} logfile" do
      expect { Keylog.new(IO.read(f)).convert }.not_to raise_error
    end
  end
end
