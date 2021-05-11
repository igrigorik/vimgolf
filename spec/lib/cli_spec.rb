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

  it "runs 'vimgolf setup'" do
    ClimateControl.modify HOME: '/home/myuser' do
      VimGolf::CLI.initialize_ui
      expect(VimGolf.ui).to receive(:ask)
        .with("\nPaste your VimGolf key:")
        .and_return('abcdefghijklmnopqrstuvwxyz012345')
      expect(FileUtils).to receive(:mkdir_p).with('/home/myuser/.vimgolf')
      expect(FileUtils).to receive(:mkdir_p).with('/home/myuser/.vimgolf/put')
      expect(VimGolf::Config).to receive(:save)
        .with({ 'key' => 'abcdefghijklmnopqrstuvwxyz012345' })

      out = capture_stdout do
        VimGolf::CLI.start(['setup'])
      end
      expect(out).to include('Saved. Happy golfing!')
    end
  end

  it "runs 'vimgolf version'" do
    out = capture_stdout do
      VimGolf::CLI.start(['version'])
    end

    expect(out).to match(/^Client \d+(\.\d+)+$/)
  end
end
