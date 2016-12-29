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

end
