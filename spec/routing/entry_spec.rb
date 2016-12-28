require 'spec_helper'

describe "routing to entries" do
  it "routes POST to /entry to challenges#create" do
    { :post => "/entry" }.should route_to(
      :controller => "entry",
      :action => "create"
    )
  end
end