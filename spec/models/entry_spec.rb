require 'spec_helper'

describe Entry do

  it { should have_fields(:script) }
  it { should validate_length_of(:script) }

  let(:c) do
    User.create(
      name: "Bill Nye",
      nickname: "The Science Guy",
      provider: "foo",
      image: "bar",
      uid: 12345
    )
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
    c.save.should be true

    c.entries.size.should == 2
    c.entries.first.created_at.should_not be_nil
    c.entries.first.user.should == User.first
  end
end
