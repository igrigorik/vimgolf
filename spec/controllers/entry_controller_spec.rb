require 'spec_helper'

describe EntryController do

  it "should create an entry for an existing challenge without login" do
    User.create(
      name: "Bill Nye",
      nickname: "The Science Guy",
      provider: "foo",
      image: "bar",
      uid: "baz"
    )
    c = Challenge.new({
                        :title => :test,
                        :description => :test,
                        :input => :a,
                        :output => :b,
                        :diff => :c
    })

    c.user = User.first
    c.save

    request.accept = 'application/json'
    post "create", {
      :format => :json,
      :challenge_id => c.id.to_s,
      :entry => 'a'*50,
      :apikey => User.first.key
    }

    response.status.should == 200
    ActiveSupport::JSON.decode(response.body).should include 'status'
  end

  it "should report an error for a non-existing challenge" do
    request.accept = 'application/json'
    post "create", :format => :json

    response.status.should == 400
    ActiveSupport::JSON.decode(response.body).should include 'status'
  end

end
