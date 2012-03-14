require "helper"

include VimGolf

describe VimGolf::Keylog do

  Dir['spec/fixtures/*'].each do |f|
    it "should parse #{f} logfile" do
      lambda { Keylog.convert(IO.read(f)) }.should_not raise_error
    end

    it "should score #{f} logfile" do
      lambda { Keylog.score(IO.read(f)) }.should_not raise_error
    end
  end
end
