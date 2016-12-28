require 'spec_helper'

describe Entry do

  it { should have_fields(:script) }
  it { should validate_length_of(:script) }

  let(:c) do
    c = Challenge.new({
                        :title => :test,
                        :description => :test,
                        :input => :a,
                        :output => :b,
                        :diff => :c
    })
    c.user = User.first
    c.save
    c
  end

  it "should be embedded inside of Challenge" do
    e1 = Entry.new(:script => :a, :created_at => Time.now, :user => c.user)
    e2 = Entry.new(:script => :b, :created_at => Time.now, :user => c.user)

    c.entries << e1
    c.entries << e2
    c.save.should be_true

    c.entries.size.should == 2
    c.entries.first.created_at.should_not be_nil
    c.entries.first.user.should == User.first
  end

  it "should store unique lists of upvotes & downvotes" do
    e = Entry.new(:script => :a, :created_at => Time.now, :user => c.user)
    e.upvote :igrigorik
    e.upvote :test_user
    e.upvote :igrigorik

    e.downvote :other_user
    e.downvote :other_user

    c.entries << e
    c.save

    e = c.entries.first
    e.upvotes.should == [:igrigorik, :test_user]
    e.downvotes.should == [:other_user]

    e.voted?(:other_user).should be_true
    e.voted?(:igrigorik).should be_true
    e.voted?(:nope).should be_false
  end
end
