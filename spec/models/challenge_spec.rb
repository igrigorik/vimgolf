require 'spec_helper'

describe Challenge do

  it { is_expected.to have_fields(:title, :description, :input, :output, :diff) }

  it { is_expected.to validate_presence_of(:title) }
  it { is_expected.to validate_presence_of(:description) }

  it { is_expected.to validate_length_of(:input) }
  it { is_expected.to validate_length_of(:output) }
  it { is_expected.to validate_length_of(:diff) }

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

    expect(c.user).to eq(u)
    # u.challenges = [c]
    p u.save

    p u
    expect(u.challenges.size).to eq(1)
    expect(u.challenges.first).to eq(c)

    u = User.find(u.id)
    p u
    expect(u.challenges.size).to eq(1)

  end
end
