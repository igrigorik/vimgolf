require "helper"

describe VimGolf do
  it "provides VimGolf errors" do
    VimGolf::Error.should be
  end

  it "provides a help prompt" do
    out = capture_stdout do
      VimGolf::CLI.start(["help"])
    end

    out.should include("setup")
    out.should include("launch")
  end

end