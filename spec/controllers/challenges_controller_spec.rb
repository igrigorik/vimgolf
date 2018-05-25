require 'spec_helper'

describe ChallengesController do

  it "should require login to create challenge" do
    request.accept = 'application/json'
    post "create", :format => :json

    expect(response.status).to eq(302)
  end

  it "should allow download of challenge without login" do
    create(
      :challenge,
      title: "foo",
      description: "bar",
      input: "baz",
      output: "qux",
      diff: "hoge"
    )
    request.accept = 'application/json'
    get "show", params: { :id => Challenge.first.id.to_s, :format => :json }

    expect(response.status).to eq(200)
  end

end
