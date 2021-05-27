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
  s.description = "Real Vim ninjas count every keystroke - do you? Pick a challenge on vimgolf.com, fire up Vim, and show us what you got."
  s.licenses    = ["MIT"]

  s.rubyforge_project = "vimgolf"

  s.required_ruby_version = ">= 2.0"

  s.add_runtime_dependency "thor", "~> 1.0", ">= 1.0.1"
  s.add_runtime_dependency "json_pure", "~> 2.3", ">= 2.3.1"
  s.add_runtime_dependency "highline", "~> 2.0", ">= 2.0.3"

  s.add_development_dependency "rspec", "~> 3.7", ">= 3.7.0"
  s.add_development_dependency "rake", "~> 12.3", ">= 12.3.1"
  s.add_development_dependency "climate_control", "~> 0.2"
  s.add_development_dependency "diff-lcs", "~> 1.4"
  s.add_development_dependency "rexml", "~> 3.1.9"
  s.add_development_dependency "webmock", "~> 3.13"

  s.files         = Dir.glob("{bin,lib}/**/*") + %w(README.md)
  s.executables   = ["vimgolf"]
  s.require_paths = ["lib"]

   s.post_install_message = %{
------------------------------------------------------------------------------
Thank you for installing vimgolf-#{Vimgolf::VERSION}.

0.1.3: custom vimgolf .vimrc file to help level the playing field
0.2.0: proxy support, custom diffs + proper vimscript parser/scoring
0.3.0: improve windows support, switch to YAML to remove c-ext dependency
0.4.0: improved diff/retry CLI, emacs support: http://bit.ly/yHgOPF

*NOTE*: please re-run "vimgolf setup" prior to playing!

For more information, rules & updates: http://vimgolf.com/about
------------------------------------------------------------------------------
}
end
