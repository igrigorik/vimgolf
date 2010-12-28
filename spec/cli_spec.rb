require "helper"

describe VimGolf do
  it "provides VimGolf errors" do
    VimGolf::Error.should be
  end

  it "sets up VimGolf.ui" do
    VimGolf.ui.should be_an(VimGolf::UI)
    capture_stdout do
      VimGolf::CLI.start(["help"])
    end
    VimGolf.ui.should be_an(VimGolf::CLI::UI)
  end

  it "provides a help prompt" do
    out = capture_stdout do
      VimGolf::CLI.start(["help"])
    end

    out.should include("setup")
    out.should include("launch")
  end

  describe "download / upload challenge" do
    let(:c) { VimGolf::CLI.new }

    it "should raise error on invalid challenge" do
      lambda { c.download('invalidID') }.should raise_error
    end

    it "should return type of challenge on success" do
      c.download('4d1a1c36567bac34a9000002').should == "rb"
    end

    it "should raise error on invalid upload id" do
      lambda { c.upload('invalidID') }.should raise_error
    end
  end

end