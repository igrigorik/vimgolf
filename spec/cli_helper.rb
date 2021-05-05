require 'vimgolf'
require 'stringio'

module Kernel
  def capture_stdio(input = nil)
    if input
      org_stdin = $stdin
      $stdin = StringIO.new(input)
    end
    org_stdout = $stdout
    $stdout = StringIO.new
    yield
    @out = $stdout.string
  ensure
    $stdout = org_stdout
    $stdin = org_stdin
  end
  alias capture_stdout capture_stdio
end

RSpec.configure do |config|
  config.after(:each) do
    # To imitate the initial class load before calling `VimGolf::CLI.start`
    VimGolf.ui = VimGolf::UI.new
  end
end
