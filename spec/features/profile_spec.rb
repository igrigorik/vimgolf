require "spec_helper"

feature "Display created challenge in profile" do

  before do
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

feature "Display played challenge in profile" do

  before do
    @played_zero_challenge = User.create!(
      name: "euclid",
      nickname: "played.zero.challenge",
      provider: "foo",
      image: "foo.jpg",
      uid: 1283578
    )

    @played_two_challenges = User.create!(
      name: "pythagoras",
      nickname: "played.two.challenge",
      provider: "foo",
      image: "foo.jpg",
      uid: 123588
    )

    creator = User.create!(
      name: "descartes",
      nickname: "creator",
      provider: "foo",
      image: "foo.jpg",
      uid: 123603
    )

    challenge = Challenge.new(
      :title => 'simple practical',
      :description => :test,
      :input => :a,
      :output => :b,
      :diff => :c,
      user_id: creator.id,
    )
    challenge.save!
    challenge.entries << Entry.new(
      script: "script",
      created_at: Time.now,
      updated_at: Time.now,
      user_id: @played_two_challenges.id,
      score: 10
    )

    challenge.entries << Entry.new(
      script: "script",
      created_at: Time.now,
      updated_at: Time.now,
      user_id: @played_two_challenges.id,
      score: 19
    )

    # lowest score
    challenge.entries << Entry.new(
      script: "script",
      created_at: Time.now,
      updated_at: Time.now,
      user_id: creator.id,
      score: 3
    )

    challenge_other = Challenge.new(
      :title => 'clean number',
      :description => :test,
      :input => :a,
      :output => :b,
      :diff => :c,
      user_id: creator.id,
    )
    challenge_other.save!
    challenge_other.entries << Entry.new(
      script: "script",
      created_at: Time.now,
      updated_at: Time.now,
      user_id: @played_two_challenges.id,
      score: 1
    )

  end

  context 'User did not played any challenge' do
    scenario 'it display 0 played challenges' do
      visit profile_path(@played_zero_challenge.nickname)
      expect(page).to have_text 'entered into 0 challenges'
    end
  end

  context 'User played some challenge' do
    scenario 'it display informations about challenges' do
      visit profile_path(@played_two_challenges.nickname)
      expect(page).to have_text 'entered into 2 challenges'

      expect(page).to have_text 'simple practical - 3 entries'
      expect(page).to have_text 'Best score: 3'
      expect(page).to have_text 'Best player score: 10'
      expect(page).to have_text 'Number of attempts: 2'

      expect(page).to have_text 'clean number - 1 entries'
      expect(page).to have_text 'Best score: 1'
      expect(page).to have_text 'Best player score: 1'
      expect(page).to have_text 'Number of attempts: 1'
    end
  end

end
