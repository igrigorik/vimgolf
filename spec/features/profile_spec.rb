require "spec_helper"

feature "Display created challenge in profile" do

  before(:each) do
    @created_zero_challenge = User.create!(
      name: "bill nye",
      nickname: "zero.challenge",
      provider: "foo",
      image: "foo.jpg",
      uid: 1283571
    )

    @created_one_challenge = User.create!(
      name: "bill nye",
      nickname: "one.challenge",
      provider: "foo",
      image: "foo.jpg",
      uid: 123563
    )

    challenge = Challenge.new(
      :title => 'title of challenge',
      :description => :test,
      :input => :a,
      :output => :b,
      :diff => :c,
      :user_id => @created_one_challenge.id
    )
    challenge.save
  end

  context 'User did not create any challenge' do
    scenario 'it display created challenges' do
      visit profile_path(@created_zero_challenge.nickname)
      expect(page).to have_text 'contributed 0 challenges'
    end
  end

  context 'User created one challenge' do
    scenario 'it display created challenges' do
      visit profile_path(@created_one_challenge.nickname)
      expect(page).to have_text 'contributed 1 challenges'
      expect(page).to have_text 'title of challenge - 0 entries'
    end
  end

end
