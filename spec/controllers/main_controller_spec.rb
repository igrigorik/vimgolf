require 'spec_helper'

describe MainController do
  render_views

  describe "#index" do
    context "With 0 challenges 0 users 0 entries" do
      it "display empty list" do
        get "index"

        expect(response.status).to eq(200)
        expect(response.body).to match(/Open VimGolf challenges/)
        expect(assigns[:stats][:users]).to eq(0)
        expect(assigns[:stats][:challenges]).to eq(0)
        expect(assigns[:stats][:entries]).to eq(0)
        expect(assigns[:challenges].to_a.size).to eq(0)
      end
    end

    context "With > 50 challenges 200 users 200 entries" do
      before do
        10.times do
          create(:user)
        end
        users = User.all

        51.times do
          challenge = create(:challenge, user: users.sample)
          create(:entry, challenge: challenge, user: users.sample)
          create(:entry, challenge: challenge, user: users.sample)
        end
      end

      it "display page, prepare stats, display pagination and limit to 50" do
        get "index"

        expect(response.status).to eq(200)
        expect(response.body).to match(/Open VimGolf challenges/)

        expect(assigns[:stats][:users]).to eq(10)
        expect(assigns[:stats][:challenges]).to eq(51)
        expect(assigns[:stats][:entries]).to eq(102)
        expect(response.body).to match(/<nav class="pagination" role="navigation"/)
        expect(assigns[:challenges].to_a.size).to eq(50)
      end
    end
  end

  describe "#feed" do
    context "With 0 challenges" do
      it "display title" do
        get "feed", :format => :rss

        expect(response.status).to eq(200)
        expect(response.body).to match(/VimGolf: Latest Challenges/)
      end
    end

    context "With more than 15 challenges" do
      before do
        user = User.create!(
          name: "Bill Nye",
          nickname: "The Science Guy",
          provider: "foo",
          image: "bar",
          uid: 12345
        )

        20.times do
          Challenge.create!({
            :title => :test,
            :description => :test,
            :input => :a,
            :output => :b,
            :diff => :c,
            :user => user
          })
        end
      end

      it "limit to 15 challenges" do
        get "feed", :format => :rss

        expect(response.status).to eq(200)
        expect(response.body).to match(/VimGolf: Latest Challenges/)
        expect(assigns[:challenges].to_a.size).to eq(15)
      end
    end
  end

end
