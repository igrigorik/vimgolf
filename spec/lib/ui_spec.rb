require "cli_helper"

describe VimGolf::CLI::UI do
  before(:each) do
    VimGolf::CLI.initialize_ui
  end

  it "produces error messages" do
    out = capture_stdout and_stderr: true do
      VimGolf.ui.error('name', 'message')
    end
    expect(out).to eq("        name  message\n")
  end

  it "produces warning messages" do
    out = capture_stdout do
      VimGolf.ui.warn('name', 'message')
    end
    expect(out).to eq("        name  message\n")
  end

  it "produces info messages" do
    out = capture_stdout do
      VimGolf.ui.info('name', 'message')
    end
    expect(out).to eq("        name  message\n")
  end

  it "produces debug messages" do
    out = capture_stdout do
      VimGolf.ui.debug('ignored without environment set')
      ClimateControl.modify DEBUG: 'yes' do
        VimGolf.ui.debug(['an', 'array'])
        VimGolf.ui.debug('name', { payload: 'message' })
      end
      VimGolf.ui.debug('back to ignoring it')
    end
    expect(out).to eq("[\"an\", \"array\"]\n        name  {:payload=>\"message\"}\n")
  end

  it "asks questions" do
    expect_any_instance_of(HighLine).to receive(:ask).and_return(:retry)
    result = VimGolf.ui.ask_question "Choice> ", type: :warn, choices: [:diff, :retry, :quit]
    expect(result).to be(:retry)
  end

  it "colors strings" do
    expect(VimGolf.ui.color_string('message', :info)).to eq("\e[32mmessage\e[0m")
    expect(VimGolf.ui.color_string('message', :warn)).to eq("\e[33mmessage\e[0m")
    expect(VimGolf.ui.color_string('message', :error)).to eq("\e[31mmessage\e[0m")
    expect(VimGolf.ui.color_string('message', :debug)).to eq("\e[36mmessage\e[0m")
  end

  it "prints exceptions" do
    out = capture_stdout and_stderr: true do
      begin
        1 / 0
      rescue StandardError => e
        ClimateControl.modify DEBUG: 'yes' do
          VimGolf.ui.print_exception(e)
        end
      end
    end
    expect(out).to match(/^ZeroDivisionError  divided by 0$/)
    # Match a backtrace. capture_stdio known to be present in the stack.
    expect(out).to match(%r{\bspec/cli_helper\.rb:\d+:in `capture_stdio'$})
  end
end
