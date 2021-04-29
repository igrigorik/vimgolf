require 'spec_helper'

describe ChallengesController do
  describe "#create" do
    it "should require login to create challenge" do
      post "create", :format => :json

      expect(response.status).to eq(302)
    end
  end

  describe "#show" do
    context "json format" do
      let(:challenge) do
        create(
          :challenge,
          title: "foo",
          description: "bar",
          input: "baz",
          input_type: "baz_type",
          output: "qux",
          output_type: "qux_type",
          diff: "hoge"
        )
      end
      it "should allow download of challenge without login" do
        get "show", params: { id: challenge.urlkey }, :format => :json

        expect(response.status).to eq(200)
      end

      it "Return object with defined attributes" do
        get "show", params: { id: challenge.urlkey }, :format => :json

        json = ActiveSupport::JSON.decode(response.body)

        expect(json).to eq(
          {
            "in" => { "data" => "baz", "type" => "baz_type" },
            "out" => { "data" => "qux", "type" => "qux_type" },
            "client" => Vimgolf::VERSION
          }
        )
      end
    end

    context "html format" do
      render_views

      let(:challenge) do
        create(
          :challenge,
          title: "foo",
          description: "bar",
          input: "baz",
          input_type: "baz_type",
          output: "qux",
          output_type: "qux_type",
          diff: "hoge"
        )
      end

      before do
        challenge.entries <<  build(:entry)
      end

      it "works with html views" do
        get "show", params: { :id => challenge.urlkey }
        expect(response.status).to eq(200)
      end
    end
  end

  describe "#user" do
    context "html format" do
      render_views

      let(:user1) { create(:user) }
      let(:user2) { create(:user) }
      let(:challenge) { create(:challenge) }

      before do
        challenge.entries << build(:entry, user: user1, score: 19, created_at: Time.new(2018, 3, 20))
        challenge.entries << build(:entry, user: user2, score: 17, created_at: Time.new(2018, 3, 19))
        challenge.entries << build(:entry, user: user1, score: 11, created_at: Time.new(2018, 3, 18))
      end

      it "works with html views" do
        get "user", params: { id: challenge.urlkey, username: user1.nickname }
        expect(response.status).to eq(200)
      end
    end
  end
end
