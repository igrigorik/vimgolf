require 'spec_helper'

describe "routing to challenges" do
  it "routes /challenges/:id to challenges#show for challenge id" do
    { :get => "/challenges/abc" }.should route_to(
      :controller => "challenges",
      :action => "show",
      :id => "abc"
    )
  end
end