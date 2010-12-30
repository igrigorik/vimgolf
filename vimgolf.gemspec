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
Thank you for installing vimgolf-#{Vimgolf::VERSION}. As of 0.1.3, we are shipping a custom
vimgolf .vimrc file for each challenge to help level the playing field.

For more information, rules & updates: http://vimgolf.com/about
------------------------------------------------------------------------------
}
end
