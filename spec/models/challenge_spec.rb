require 'spec_helper'

describe Challenge do

  it { should have_fields(:title, :description, :input, :output, :diff) }

  it { should validate_presence_of(:title) }
  it { should validate_presence_of(:description) }

  it { should validate_length_of(:input) }
  it { should validate_length_of(:output) }
  it { should validate_length_of(:diff) }

  it "should maintain associations between user and challenges" do
    u = User.create({
                   :provider => :fake,
                   :uid => 1,
                   :nickname => :test,
                   :name => 'Test User',
                   :image => 'fake',
                   :description => 'fake',
                   :location => 'fake'
    })

    c = Challenge.new({
      :title => :test,
      :description => :test,
      :input => :a,
      :output => :b,
      :diff => :c
    })

    c.user = u
    c.save

    c.user.should == u
    # u.challenges = [c]
    p u.save

    p u
    u.challenges.size.should == 1
    u.challenges.first.should == c

    u = User.find(u.id)
    p u
    u.challenges.size.should == 1

  end
end
