require "cli_helper"
require "tmpdir"

TESTDATA = File.join(File.dirname(__FILE__), 'testdata')
MOCK_VIM = File.join(File.dirname(__FILE__), 'mock_bin', 'mock_vim.rb')
MOCK_DIFF = File.join(File.dirname(__FILE__), 'mock_bin', 'mock_diff.sh')

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

  it "sets up VimGolf.ui when initialize_ui explicitly called" do
    expect(VimGolf.ui).to be_an(VimGolf::UI)
    VimGolf::CLI.initialize_ui
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

  it "runs 'vimgolf setup' with incorrect key" do
    ClimateControl.modify HOME: '/home/myuser' do
      VimGolf::CLI.initialize_ui
      expect(VimGolf.ui).to receive(:ask)
        .with("\nPaste your VimGolf key:")
        .and_return('bogus-key')

      out = capture_stdout and_stderr: true do
        VimGolf::CLI.start(['setup'])
      end
      expect(out).to include('Invalid key, please double check your key on vimgolf.com')
    end
  end

  it "runs 'vimgolf version'" do
    out = capture_stdout do
      VimGolf::CLI.start(['version'])
    end

    expect(out).to match(/^Client \d+(\.\d+)+$/)
  end

  it "runs 'vimgolf local'" do
    Dir.mktmpdir('vimgolf_test_') do |tmpdir|
      ClimateControl.modify HOME: tmpdir do
        VimGolf::CLI.initialize_ui
        stub_const('VimGolf::CLI::GOLFVIM', "#{RbConfig.ruby} #{MOCK_VIM}")

        expect(VimGolf.ui).to receive(:ask_question)
          .with('Choice> ', type: :warn, choices: [:retry, :quit])
          .and_return(:quit)

        out = capture_stdout do
          VimGolf::CLI.start(
            [
              'local',
              File.join(TESTDATA, 'input.txt'),
              File.join(TESTDATA, 'output.txt')
            ]
          )
        end

        expect(out).to include <<~EDQ
          Here are your keystrokes:
          7<C-A>atest<Home>Bill <Esc>ZZ

          Success! Your output matches. Your score: 16
        EDQ
      end
    end
  end

  it "runs 'vimgolf local' with incorrect output and shows diff" do
    Dir.mktmpdir('vimgolf_test_') do |tmpdir|
      ClimateControl.modify HOME: tmpdir do
        VimGolf::CLI.initialize_ui
        stub_const('VimGolf::CLI::GOLFVIM', "#{RbConfig.ruby} #{MOCK_VIM}")
        stub_const('VimGolf::CLI::GOLFSHOWDIFF', "/bin/sh #{MOCK_DIFF}")

        expect(VimGolf.ui).to receive(:ask_question)
          .with('Choice> ', type: :warn, choices: [:diff, :retry, :quit])
          .and_return(:diff, :quit)

        out = capture_stdout and_stderr: true do
          VimGolf::CLI.start(
            [
              'local',
              File.join(TESTDATA, 'input.txt'),
              File.join(TESTDATA, 'mismatch.txt')
            ]
          )
        end

        expect(out).to include <<~EDQ
          Here are your keystrokes:
          7<C-A>atest<Home>Bill <Esc>ZZ

          Uh oh, looks like your entry does not match the desired output.
          Your score for this failed attempt was: 16
          [d] Show diff
          [r] Retry the current challenge
          [q] Quit vimgolf
          Showing vimdiff of your attempt (left) and correct output (right)
          [d] Show diff
          [r] Retry the current challenge
          [q] Quit vimgolf

          Thanks for playing!
        EDQ

        expect(File.read(File.join(tmpdir, 'diff-output.txt'))).to include <<~EDQ
          @@ -1 +1 @@
          -Bill nye  says 010test
          +Naomi nye  says 008testing
        EDQ
      end
    end
  end
end
