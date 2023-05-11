require "spec_helper"

feature "Deleting a challenge" do
  include OmniAuthHelper

  before(:each) do
    mock_omni_auth
  end

  scenario "as the owner of the challenge" do
    user = User.create!(
      nickname: ADMINS.first,
      name: "foo",
      uid: "123545",
      image: "bar",
      provider: "twitter"
    )
    Challenge.create!(
      title: "challenge1",
      description: "a sample challenge",
      input: "aa",
      input_type: "txt",
      output: "bb",
      output_type: "txt",
      diff: "aabb",
      user_id: user.id
    )

    visit root_path

    click_link "Sign in"
    click_link "Sign in with Twitter"
    click_link "challenge1"
    click_button "Delete Challenge"

    expect(Challenge.count).to eq(0)
  end

  scenario "not as the owner of the challenge" do
    user = User.create!(
      nickname: "foo",
      name: "bar",
      uid: 545321,
      image: "baz",
      provider: "twitter"
    )
    Challenge.create!(
      title: "challenge1",
      description: "a sample challenge",
      input: "aa",
      input_type: "txt",
      output: "bb",
      output_type: "txt",
      diff: "aabb",
      user_id: user.id
    )

    visit root_path

    click_link "challenge1"

    expect(page).not_to have_text("Delete Challenge")
    expect(Challenge.count).to eq(1)
  end
end
