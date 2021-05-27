# frozen_string_literal: true

require 'diff/lcs'
require 'diff/lcs/ldiff'

$stdout = File.open(File.join(ENV['HOME'], 'diff-output.txt'), 'w')

exit Diff::LCS::Ldiff.run(ARGV)
