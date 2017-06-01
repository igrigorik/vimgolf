require "spec_helper"

feature "Submitting a challenge" do
  include OmniAuthHelper

  before(:each) do
    mock_omni_auth
  end

  scenario "with missing fields" do
    visit root_path

    click_link "Sign in with Twitter"

    click_link "Submit challenge"

    click_button "Create challenge"

    expect(page).to have_text "Title can't be blank"
    expect(page).to have_text "Description can't be blank"
    expect(page).to have_text "Input is too short (minimum is 1 character)"
    expect(page).to have_text "Output is too short (minimum is 1 character)"
    expect(page).to have_text "Diff is too short (minimum is 1 character)"
  end

  scenario "with properly filled out fields" do
    visit root_path

    click_link "Sign in with Twitter"

    click_link "Submit challenge"

    fill_in "Title", with: "Super hard challenge"
    fill_in "Description", with: "Turn back now; it's impossible!"
    attach_file("challenge_input", path_for_data_file("input.txt"))
    attach_file("challenge_output", path_for_data_file("output.txt"))
    attach_file("challenge_diff", path_for_data_file("diff.txt"))
    click_button "Create challenge"

    expect(current_path).to eq(challenge_path(Challenge.first))
    expect(Challenge.count).to eq(1)
  end

  def path_for_data_file(file)
    File.join(Rails.root, "/spec/data/", file)
  end
end
