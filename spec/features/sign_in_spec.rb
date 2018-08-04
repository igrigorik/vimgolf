require "spec_helper"

feature "Sign In" do
  include OmniAuthHelper

  before(:each) do
    mock_omni_auth
  end

  scenario "as a first-time user" do
    visit root_path

    click_link "Sign in with Twitter"

    expect(current_path).to eq(root_path)
    expect(page).to have_text "Welcome @the science guy"
  end

  scenario "as an existing user" do
    user = User.create!(
      nickname: "foo",
      name: "bar",
      uid: "123545",
      image: "foo",
      provider: "twitter"
    )

    visit root_path

    click_link "Sign in with Twitter"

    expect(page).to have_text "Welcome @the science guy"
    expect(user.reload.name).to eq("bill nye")
  end
end
