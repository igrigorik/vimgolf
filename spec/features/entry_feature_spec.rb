require "spec_helper"

feature "Entries for Challenges" do
  include OmniAuthHelper

  before(:each) do
    mock_omni_auth

    User.create!(
      name: "bill nye",
      nickname: "the science guy",
      provider: "foo",
      image: "foo.jpg",
      uid: 123545
    )
  end

  context '#create' do
    before(:example) do

      c = Challenge.new({
        :title => :test,
        :description => :test,
        :input => :a,
        :output => :b,
        :diff => :c
      })

      c.user = User.first
      c.save

      entry = Entry.new(
                :script => 'ddZZ',
                :score => VimGolf::Keylog.new('ddZZ').score
                )
      entry.created_at = Time.now.utc
      entry.user = User.last

      c.entries << entry

      c.save
    end

    scenario 'can comment on an entry', js: true do
      visit root_path
      click_link "Sign in with Twitter"
      click_link 'test'
      click_link 'Comment'
      fill_in 'comment_text', with: 'test comment'
      click_button 'Comment'
      expect(page).to have_css '.comment', text: 'the science guy: test comment'
      expect(page).to have_text '1 comment'
    end
  end

  scenario 'can delete an entry' do

  end

  scenario 'can create an entry for a challenge' do

  end

end
