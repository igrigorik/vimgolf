require 'spec_helper'

describe User do
  @fields = [:provider, :uid, :nickname, :name, :location, :image, :description, :key]

  it { should have_fields(*@fields) }
  it { should validate_numericality_of(:uid) }

  (@fields - [:uid, :key, :location, :description]).each do |field|
    it { should validate_presence_of(field) }
  end

  it "should create an API key on save" do
    u = User.create({
                   :provider => :fake,
                   :uid => 1,
                   :nickname => :test,
                   :name => 'Test User',
                   :image => 'fake',
                   :description => 'fake',
                   :location => 'fake'
    })

    u.key.size.should == 32
  end

end
