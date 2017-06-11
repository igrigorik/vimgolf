require "spec_helper"

describe SessionsController do
  describe "#create" do
    it "creates a user from omniauth data in headers" do
      request.env["omniauth.auth"] = {}
      request.env["omniauth.auth"]["provider"] = "twitter"
      request.env["omniauth.auth"]["uid"] = "12345"
      request.env["omniauth.auth"]["info"] = {
        email: "bill@nye.com",
        name: "Bill Nye",
        nickname: "the science guy",
        image: "me.png",
      }

      expect do
        post :create
      end.to change(User, :count).by(1)
      user = User.first
      expect(user.name).to eq("Bill Nye")
      expect(user.nickname).to eq("the science guy")
      expect(user.image).to eq("me.png")
      expect(user.provider).to eq("twitter")
      expect(user.uid).to eq("12345")
    end
  end
end
