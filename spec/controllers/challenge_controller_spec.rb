require 'spec_helper'

describe ChallengesController do

  it "should require login to create challenge" do
    request.accept = 'application/json'
    post "create", :format => :json

    response.status.should == 302
  end

  it "should allow download of challenge without login" do
    request.accept = 'application/json'
    get "show", :id => Challenge.first.id.to_s, :format => :json

    response.status.should == 200
  end

end