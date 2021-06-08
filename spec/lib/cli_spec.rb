require "cli_helper"
require "tmpdir"
require "webmock/rspec"

WebMock.disable_net_connect!

TESTDATA = File.join(File.dirname(__FILE__), 'testdata')
MOCK_VIM = File.join(File.dirname(__FILE__), 'mock_bin', 'mock_vim.rb')
MOCK_DIFF = File.join(File.dirname(__FILE__), 'mock_bin', 'mock_diff.rb')

class String
  def strip_heredoc
    gsub(/^#{scan(/^[ \t]*(?=\S)/).min}/, "".freeze)
  end
end

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

        expect(out).to include <<-EDQ.strip_heredoc
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
        stub_const('VimGolf::CLI::GOLFSHOWDIFF', "#{RbConfig.ruby} #{MOCK_DIFF} -u")

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

        expect(out).to include <<-EDQ.strip_heredoc
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

        expect(File.read(File.join(tmpdir, 'diff-output.txt'))).to include <<-EDQ.strip_heredoc
          @@ -1 +1 @@
          -Bill nye  says 010test
          +Naomi nye  says 008testing
        EDQ
      end
    end
  end

  it "runs 'vimgolf put'" do
    Dir.mktmpdir('vimgolf_test_') do |tmpdir|
      ClimateControl.modify HOME: tmpdir do
        FileUtils.mkdir_p(File.join(tmpdir, '.vimgolf'))
        File.write(
          File.join(tmpdir, '.vimgolf', 'config.yaml'),
          "---\nkey: abcdefghijklmnopqrstuvwxyz012345\n"
        )
        VimGolf::CLI.initialize_ui
        stub_const('VimGolf::CLI::GOLFVIM', "#{RbConfig.ruby} #{MOCK_VIM}")

        get_req = stub_request(:get, "#{VimGolf::GOLFHOST}/challenges/8v90deadbeef000012345678.json")
                  .to_return(
                    status: 200,
                    headers: { content_type: 'application/json' },
                    body: {
                      in: {
                        'data' => "nye  says 001\n",
                        'type' => 'txt'
                      },
                      out: {
                        'data' => "Bill nye  says 010test\n",
                        'type' => 'txt'
                      },
                      client: Vimgolf::VERSION
                    }.to_json
                  )
        post_req = stub_request(:post, "#{VimGolf::GOLFHOST}/entry.json")
                   .with(
                     body: URI.encode_www_form(
                       {
                         challenge_id: '8v90deadbeef000012345678',
                         apikey: 'abcdefghijklmnopqrstuvwxyz012345',
                         entry: "7\x01atest\x80khBill \x1b\x80\xfdaZZ"
                       }
                     ),
                     headers: { accept: 'application/json' }
                   ).to_return(
                     status: 200,
                     headers: { content_type: 'application/json' },
                     body: { status: 'ok' }.to_json
                   )

        expect(VimGolf.ui).to receive(:ask_question)
          .with('Choice> ', type: :warn, choices: [:w, :x, :retry, :quit])
          .and_return(:x)

        out = capture_stdout do
          VimGolf::CLI.start(['put', '8v90deadbeef000012345678'])
        end

        expect(get_req).to have_been_requested
        expect(post_req).to have_been_requested

        expect(out).to include <<-EDQ.strip_heredoc
          Downloading Vimgolf challenge: 8v90deadbeef000012345678
          Launching VimGolf session for challenge: 8v90deadbeef000012345678

          Here are your keystrokes:
          7<C-A>atest<Home>Bill <Esc>ZZ

          Success! Your output matches. Your score: 16
          [w] Upload result and retry the challenge
          [x] Upload result and quit
          [r] Do not upload result and retry the challenge
          [q] Do not upload result and quit
          Uploading to VimGolf...
          Uploaded entry.
          View the leaderboard: #{VimGolf::GOLFHOST}/challenges/8v90deadbeef000012345678

          Thanks for playing!
        EDQ
      end
    end
  end

  it "runs 'vimgolf put' with client version mismatch" do
    Dir.mktmpdir('vimgolf_test_') do |tmpdir|
      ClimateControl.modify HOME: tmpdir do
        VimGolf::CLI.initialize_ui
        get_req = stub_request(:get, "#{VimGolf::GOLFHOST}/challenges/8v90deadbeef000012345678.json")
                  .to_return(
                    status: 200,
                    headers: { content_type: 'application/json' },
                    body: {
                      in: {
                        'data' => "nye  says 001\n",
                        'type' => 'txt'
                      },
                      out: {
                        'data' => "Bill nye  says 010test\n",
                        'type' => 'txt'
                      },
                      client: '0.1'
                    }.to_json
                  )

        out = capture_stdout and_stderr: true do
          begin
            VimGolf::CLI.start(['put', '8v90deadbeef000012345678'])
          rescue StandardError => e
            expect(e.message).to match(
              "Uh oh, couldn't download or parse challenge, " \
              "please verify your challenge id & client version."
            )
          end
        end

        expect(get_req).to have_been_requested

        expect(out).to include <<-EDQ.strip_heredoc
          Downloading Vimgolf challenge: 8v90deadbeef000012345678
          Client version mismatch. Installed: #{Vimgolf::VERSION}, Required: 0.1.
          \t gem install vimgolf
        EDQ
      end
    end
  end
end

WebMock.allow_net_connect!
