require "spec_helper"

describe "POST /entry" do
  it "creates an entry with a valid api key and valid challenge id" do
    challenge = create(
      :challenge,
      title: "foo",
      description: "bar",
      input: "aa",
      output: "bb",
      diff: "aabb"
    )
    user = User.create!(
      name: "bill nye",
      image: "foo",
      uid: 1,
      provider: "github",
      nickname: "the science guy"
    )
    body = { entry: "aa", challenge_id: challenge.id.to_s, apikey: user.key }.to_json

    post("/entry", body, headers)

    entries = challenge.reload.entries
    expect(response).to have_http_status(200)
    expect(json_response[:status]).to eq("ok")
    expect(entries.count).to eq(1)
    expect(entries.first.script).to eq("aa")
    expect(entries.first.score).to eq(2)
    expect(entries.first.created_at).not_to be nil
    expect(entries.first.user).to eq(user)
  end

  it "fails for an invalid challenge id" do
    challenge = create(
      :challenge,
      title: "foo",
      description: "bar",
      input: "aa",
      output: "bb",
      diff: "aabb"
    )
    invalid_challenge_id = BSON::ObjectId.from_string(challenge.id.to_s.reverse).to_s
    body = { entry: "foo", challenge_id: invalid_challenge_id, apikey: "a" }.to_json

    post("/entry", body, headers)

    expect(response).to have_http_status(400)
    expect(json_response[:status]).to eq("failed")
  end

  it "fails with an invalid api key" do
    challenge = create(
      :challenge,
      title: "foo",
      description: "bar",
      input: "aa",
      output: "bb",
      diff: "aabb"
    )
    invalid_key = User.create!(
      name: "bill nye",
      image: "foo",
      uid: 1,
      provider: "github",
      nickname: "the science guy"
    ).key.reverse
    body = { entry: "foo", challenge_id: challenge.id, apikey: invalid_key }.to_json

    post("/entry", body, headers)

    expect(response).to have_http_status(400)
    expect(json_response[:status]).to eq("failed")
  end

  it "fails with an empty api key" do
    body = { entry: "foobar", challenge_id: 1, apikey: "" }.to_json

    post("/entry", body, headers)

    expect(response).to have_http_status(400)
    expect(json_response[:status]).to eq("failed")
  end

  it "fails with a missing challenge id" do
    body = { entry: "foobar" }.to_json

    post("/entry", body, headers)

    expect(response).to have_http_status(400)
    expect(json_response[:status]).to eq("failed")
  end

  it "fails when you're cheating" do
    body = { entry: "a" }.to_json

    post("/entry", body, headers)

    expect(response).to have_http_status(400)
    expect(json_response[:status]).to eq("failed")
  end

  def json_response
    JSON.parse(response.body, symbolize_names: true)
  end

  def headers
    {
      "CONTENT_TYPE" => "application/json",
      "HTTP_ACCEPT" => "application/json"
    }
  end
end
