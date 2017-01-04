require 'spec_helper'

describe "routing to challenges" do
  it "routes /challenges/:id to challenges#show for challenge id" do
    expect({ :get => "/challenges/abc" }).to route_to(
      :controller => "challenges",
      :action => "show",
      :id => "abc"
    )
  end
end