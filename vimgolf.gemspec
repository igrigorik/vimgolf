# -*- encoding: utf-8 -*-
$:.push File.expand_path("../lib", __FILE__)
require "vimgolf/version"

Gem::Specification.new do |s|
  s.name        = "vimgolf"
  s.version     = Vimgolf::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = ["Ilya Grigorik"]
  s.email       = ["ilya@igvita.com"]
  s.homepage    = "http://github.com/igrigorik/vimgolf"
  s.summary     = "CLI client for vimgolf.com"
  s.description = s.summary

  s.rubyforge_project = "vimgolf"
  s.add_dependency "thor", ">= 0.14.6"
  s.add_dependency "json"
  s.add_dependency "highline"

  s.add_development_dependency "rspec"

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = ["lib"]

   s.post_install_message = %{
------------------------------------------------------------------------------
Thank you for installing vimgolf-#{Vimgolf::VERSION}. 

0.1.3: custom vimgolf .vimrc file to help level the playing field
0.2.0: proxy support, custom diffs + proper vimscript parser/scoring
0.3.0: improve windows support, switch to YAML to remove c-ext dependency

*NOTE*: please re-run "vimgolf setup" prior to playing!

For more information, rules & updates: http://vimgolf.com/about
------------------------------------------------------------------------------
}
end
