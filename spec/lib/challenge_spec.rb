require "spec_helper"
require "tmpdir"
require "fileutils"

describe VimGolf::Challenge do

  before :each do
    @dir = Dir.mktmpdir("vimgolf_test_")
    VimGolf::Challenge.path(@dir)
  end

  after :each do
    FileUtils.remove_entry_secure(@dir)
  end

  it "should raise error on invalid challenge" do
    lambda { VimGolf::Challenge.new('invalidID').download }.should raise_error(RuntimeError)
  end

  it "should return type of challenge on success" do
    challenge = VimGolf::Challenge.new('4d1a1c36567bac34a9000002')
    challenge.download
    challenge.type.should == "rb"
  end

  it "should raise error on invalid upload id" do
    lambda { VimGolf::Challenge.new('invalidID').upload }.should raise_error(RuntimeError)
  end
end


