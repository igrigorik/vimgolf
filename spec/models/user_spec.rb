require 'spec_helper'

describe User do
  describe 'Validations' do
    it do
      fields = [
        :provider,
        :uid,
        :nickname,
        :name,
        :location,
        :image,
        :description,
        :key
      ]

      should have_fields(*fields)
    end

    it { should validate_numericality_of(:uid) }

    it { should validate_presence_of(:provider) }
    it { should validate_presence_of(:nickname) }
    it { should validate_presence_of(:image) }
    it { should validate_presence_of(:name) }
  end

  describe 'Callbacks' do
    context 'on before_save' do
      it 'should create an API key on save' do
        user = User.create(
          provider: :fake,
          uid: 1,
          nickname: :test,
          name: 'Test User',
          image: :fake,
          description: :fake,
          location: :fake
        )

        expect(user.key).not_to be nil
      end
    end
  end

  describe 'Associations' do
    it 'references many challenges' do
      association = described_class.reflect_on_association(:challenges)

      expect(association.macro).to eq :has_many
    end
  end

  describe '#admin?' do
    context 'when user is included in the admin list' do
      it 'return true' do
        user = User.new(nickname: 'igrigorik')

        expect(user.admin?).to be true
      end
    end

    context 'when user is not included in the admin list' do
      it 'return false' do
        user = User.new(nickname: 'nelsonsar')

        expect(user.admin?).to be false
      end
    end
  end
end
