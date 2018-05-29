require "spec_helper"

feature "Entries for Challenges" do
  include OmniAuthHelper

  before(:each) do
    User.create!(
      name: "bill nye",
      nickname: "the science guy",
      provider: "foo",
      image: "foo.jpg",
      uid: 123545
    )

    challenge = Challenge.new(
      :title => :test,
      :description => :test,
      :input => :a,
      :output => :b,
      :diff => :c
    )
    challenge.user = User.first
    challenge.save
  end

  context 'Entry exists on a Challenge, user is the owner' do
    before(:example) do
      mock_omni_auth

      challenge = Challenge.first

      entry = Entry.new(
        :script => 'ddZZ',
        :score => VimGolf::Keylog.new('ddZZ').score
      )
      entry.created_at = Time.now.utc
      entry.user = User.last

      challenge.entries << entry
      challenge.save

      expect(entry.user).to eq(challenge.user)
    end

    context '#comment' do
      scenario 'can comment on an entry', js: true do
        visit root_path
        click_link "Sign in with Twitter"
        click_link 'test'
        click_link 'Comment'
        fill_in 'comment_text', with: 'test comment'
        expect{ click_button 'Comment' }.to change{ Challenge.first.entries.first.comments.count }.from(0).to(1)
        expect(page).to have_css '.comment', text: 'the science guy: test comment'
        expect(page).to have_text '1 comment'
      end
    end

    context '#destroy' do
      scenario 'can delete an entry', js: true do
        visit root_path
        click_link "Sign in with Twitter"
        click_link 'test'
        click_link 'Comment / Edit'
        expect{ click_link 'Delete Entry' }.to change{ Challenge.first.entries.count }.from(1).to(0)
        expect(page).to have_text '0 entries'
      end
    end
  end

  context 'Entry exists on a Challenge, user is a participator' do
    before(:example) do
      mock_omni_auth_participator

      challenge = Challenge.first

      participator = User.create!(
        name: "zelda",
        nickname: "Z",
        provider: "foo",
        image: "foo.jpg",
        uid: 1235456
      )

      entry = Entry.new(
        :script => 'ddZZ',
        :score => VimGolf::Keylog.new('ddZZ').score,
        :created_at => Time.now.utc,
        :user_id => participator.id
      )

      challenge.entries << entry

      expect(entry.user).not_to eq(challenge.user)
    end

    context '#comment' do
      scenario 'can comment on an entry', js: true do
        visit root_path
        click_link "Sign in with Twitter"
        click_link 'test'
        click_link 'Comment'
        fill_in 'comment_text', with: 'test comment participator'
        expect{ click_button 'Comment' }.to change{ Challenge.first.entries.first.comments.count }.from(0).to(1)
        expect(page).to have_css '.comment', text: 'Z: test comment participator'
        expect(page).to have_text '1 comment'
      end
    end

    context '#destroy' do
      scenario 'can delete an entry', js: true do
        visit root_path
        click_link "Sign in with Twitter"
        click_link 'test'
        click_link 'Comment / Edit'
        expect{ click_link 'Delete Entry' }.to change{ Challenge.first.entries.count }.from(1).to(0)
        expect(page).to have_text '0 entries'
      end
    end
  end
end
