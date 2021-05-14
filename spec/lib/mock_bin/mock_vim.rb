# frozen_string_literal: true

require 'optparse'

orig_argv = "#{$PROGRAM_NAME} #{ARGV.join(' ')}"
scriptout = nil

OptionParser.new do |opts|
  opts.on('-Z', 'Restricted mode')
  opts.on('-n', 'No swapfile')
  opts.on('--noplugin', 'Disable plugin initialization')
  opts.on('-i VIMINFO', 'Path to viminfo file')
  opts.on('-u VIMRC', 'Path to the vimrc file')
  opts.on('-U GVIMRC', 'Path to the gvimrc file')
  opts.on('--nofork', "Don't fork (for GVIM)")
  opts.on('-W SCRIPTOUT', 'Path to the scriptout file') do |s|
    scriptout = s
  end
end.parse!

raise "missing scriptout: #{orig_argv}" unless scriptout

# Filter out +0 argument.
args = ARGV.reject { |arg| arg.start_with?('+') }

raise "invalid args: #{orig_argv}" if args.length != 1

# Transform input as expected for testdata.
fname, = args
contents = File.read(fname)
contents.sub!('01', '10test')
File.write(fname, "Bill #{contents}")

# Write scriptout file with sequence of Vim keystrokes.
IO.binwrite(scriptout, "7\x01atest\x80khBill \x1b\x80\xfdaZZ")
