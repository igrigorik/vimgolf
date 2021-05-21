require 'vimgolf'
require 'stringio'
require 'climate_control'

module Kernel
  def capture_stdio(input = nil, and_stderr: false)
    if input
      org_stdin = $stdin
      $stdin = StringIO.new(input)
    end
    org_stdout = $stdout
    $stdout = StringIO.new
    if and_stderr
      org_stderr = $stderr
      $stderr = $stdout
    end
    yield
    @out = $stdout.string
  ensure
    $stdout = org_stdout
    $stdin = org_stdin
    $stderr = org_stderr if and_stderr
  end
  alias capture_stdout capture_stdio
end

RSpec.configure do |config|
  config.after(:each) do
    # To imitate the initial class load before calling `VimGolf::CLI.start`
    VimGolf::CLI.reset_ui
  end
end
