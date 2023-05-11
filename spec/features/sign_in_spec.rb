require "spec_helper"

feature "Sign In" do
  include OmniAuthHelper

  before(:each) do
    mock_omni_auth
    mock_omni_auth_github
  end

  context "as a first-time user" do
    before do
      @expected_twitter_link = "/auth/twitter?x_auth_access_type=read&use_authorize=true"
      @expected_github_link = "/auth/github"
    end

    scenario "with Twitter" do
      visit root_path

      click_link "Sign in"

      within(".auth-buttons") do
        expect(page).to have_link("Sign in with Twitter", href: @expected_twitter_link)
        expect(page).to have_link("Sign in with Github", href: @expected_github_link)
      end

      expect(current_path).to eq(login_path)
      click_link "Sign in with Twitter"

      expect(current_path).to eq(root_path)
      expect(page).to have_text "Welcome @the science guy"
    end

    scenario "with Github" do
      visit root_path

      click_link "Sign in"

      within(".auth-buttons") do
        expect(page).to have_link("Sign in with Twitter", href: @expected_twitter_link)
        expect(page).to have_link("Sign in with Github", href: @expected_github_link)
      end

      expect(current_path).to eq(login_path)
      click_link "Sign in with Github"

      expect(current_path).to eq(root_path)
      expect(page).to have_text "Welcome @the god coder"
    end
  end

  context "as an existing user" do
    before do
      @expected_twitter_link = "/auth/twitter?x_auth_access_type=read&use_authorize=true"
      @expected_github_link = "/auth/github"
    end
    scenario "with Twitter" do
      user = User.create!(
        nickname: "foo",
        name: "bar",
        uid: "123545",
        image: "foo",
        provider: "twitter"
      )

      visit root_path

      click_link "Sign in"

      within(".auth-buttons") do
        expect(page).to have_link("Sign in with Twitter", href: @expected_twitter_link)
        expect(page).to have_link("Sign in with Github", href: @expected_github_link)
      end

      expect(current_path).to eq(login_path)
      click_link "Sign in with Twitter"

      expect(current_path).to eq(root_path)
      expect(page).to have_text "Welcome @the science guy"
      expect(user.reload.name).to eq("bill nye")
    end

    scenario "with Github" do
      user = User.create!(
        nickname: "foo",
        name: "bar",
        uid: "521343",
        image: "foo",
        provider: "github"
      )
      visit root_path

      click_link "Sign in"

      within(".auth-buttons") do
        expect(page).to have_link("Sign in with Twitter", href: @expected_twitter_link)
        expect(page).to have_link("Sign in with Github", href: @expected_github_link)
      end

      expect(current_path).to eq(login_path)
      click_link "Sign in with Github"

      expect(current_path).to eq(root_path)
      expect(page).to have_text "Welcome @the god coder"
      expect(user.reload.name).to eq("Linus Torvalds")
    end
  end
end
