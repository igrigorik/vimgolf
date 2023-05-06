require "spec_helper"

describe SessionsController do
  describe "#create" do
    context "with Twitter authentication" do
      it "creates a user from Twitter omniauth data" do
        request.env["omniauth.auth"] = {
          "provider" => "twitter",
          "uid" => "12345",
          "info" => {
            "email" => "bill@nye.com",
            "name" => "Bill Nye",
            "nickname" => "the science guy",
            "image" => "me.png"
          }
        }

        expect do
          post :create, params: { provider: 'twitter' }
        end.to change(User, :count).by(1)

        user = User.first
        expect(user.name).to eq("Bill Nye")
        expect(user.nickname).to eq("the science guy")
        expect(user.image).to eq("me.png")
        expect(user.provider).to eq("twitter")
        expect(user.uid).to eq(12345)
      end
    end

    context "with GitHub authentication" do
      it "creates a user from GitHub omniauth data" do
        request.env["omniauth.auth"] = {
          "provider" => "github",
          "uid" => "67890",
          "info" => {
            "email" => "linustorvalds@example.com",
            "name" => "Linus Torvalds",
            "nickname" => "the god coder",
            "image" => "avatar.png"
          }
        }

        expect do
          post :create, params: { provider: 'github' }
        end.to change(User, :count).by(1)

        user = User.last
        expect(user.name).to eq("Linus Torvalds")
        expect(user.nickname).to eq("the god coder")
        expect(user.image).to eq("avatar.png")
        expect(user.provider).to eq("github")
        expect(user.uid).to eq(67890)
      end
    end
  end
end
