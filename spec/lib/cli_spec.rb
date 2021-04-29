require "cli_helper"

describe VimGolf do
  it "provides VimGolf errors" do
    expect(VimGolf::Error).to be
  end

  it "sets up VimGolf.ui" do
    expect(VimGolf.ui).to be_an(VimGolf::UI)
    capture_stdout do
      VimGolf::CLI.start(["help"])
    end
    expect(VimGolf.ui).to be_an(VimGolf::CLI::UI)
  end

  it "provides a help prompt" do
    out = capture_stdout do
      VimGolf::CLI.start(["help"])
    end

    expect(out).to include("setup")
    expect(out).to include("launch")
  end
end
