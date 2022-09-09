require 'spec_helper'

describe Entry do
  let(:user) { create(:user) }

  describe 'Associations' do
    it 'is referenced by one user' do
      association = described_class.reflect_on_association(:user)

      expect(association.macro).to eq :belongs_to
    end

    it 'is has many comments' do
      association = described_class.reflect_on_association(:comments)

      expect(association.macro).to eq :has_many
    end

    it 'belongs to challenge' do
      association = described_class.reflect_on_association(:challenge)

      expect(association.macro).to eq :belongs_to
    end
  end

  describe '#owned_by?' do
    context 'entry was created by given user' do
      it 'return true' do
        entry = build(:entry, user: user)

        expect(entry.owned_by?(user)).to be true
      end
    end

    context 'entry was created by another user' do
      it 'return false' do
        entry = build(:entry, user: build(:user))

        expect(entry.owned_by?(user)).to be false
      end
    end
  end

  describe 'Validations' do
    it 'is not valid without script' do
      entry = build(:entry, script: '')

      expect(entry).to_not be_valid
    end

    it 'is not valid if script size is too long' do
      script = 'a' * (MAX_FILESIZE + 1)
      entry = build(:entry, script: script)

      expect(entry).to_not be_valid
    end
  end
end
