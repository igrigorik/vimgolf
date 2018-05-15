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
      let(:challenge) {
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
      }
      it "should allow download of challenge without login" do
        get "show", params: { id: challenge.id.to_s }, :format => :json

        expect(response.status).to eq(200)
      end

      it "Return object with defined attributes" do
        get "show", params: { id: challenge.id.to_s }, :format => :json

        json = ActiveSupport::JSON.decode(response.body)

        expect(json).to eq({
          "in"=>{"data"=>"baz", "type"=>"baz_type"},
          "out"=>{"data"=>"qux", "type"=>"qux_type"},
          "client"=> Vimgolf::VERSION
        })
      end
    end

    context "html format" do
      render_views

      let(:challenge) {
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
      }

      before do
        challenge.entries <<  build(:entry)
      end

      it "works with html views" do
        get "show", params: { :id => challenge.id.to_s }
        expect(response.status).to eq(200)
      end
    end
  end

end
