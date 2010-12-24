require 'rubygems'
require 'bundler/setup'

require 'rspec'
require 'lib/vimgolf'


module Kernel
  def capture_stdio(input = nil, &block)
    require 'stringio'
    org_stdin, $stdin = $stdin, StringIO.new(input) if input
    org_stdout, $stdout = $stdout, StringIO.new
    yield
    return @out = $stdout.string
  ensure
    $stdout = org_stdout
    $stdin = org_stdin
  end
  alias capture_stdout capture_stdio
end

RSpec.configure do |config|
  # config.include RSpec::Helpers
  # config.include RSpec::GitRepo
  # config.extend RSpec::Helpers::SemanticNames
end
