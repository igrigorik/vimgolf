require 'spec_helper'

describe EntryOther do
  let(:user) { create(:user) }

  describe 'Associations' do
    it 'is referenced by one user' do
      association = described_class.reflect_on_association(:user)
      expect(association.macro).to eq :belongs_to
    end

    it 'is has many comments' do
      association = described_class.reflect_on_association(:comments)
      expect(association.macro).to eq :embeds_many
    end

    it 'is embedded in challenge' do
      association = described_class.reflect_on_association(:challenge)
      expect(association.macro).to eq :belongs_to
    end
  end

  describe '#owned_by?' do
    context 'entry was created by given user' do
      it 'return true' do
        entry = build(:entry_other, user: user)

        expect(entry.owned_by?(user)).to be true
      end
    end

    context 'entry was created by another user' do
      it 'return false' do
        entry = build(:entry_other, user: build(:user))

        expect(entry.owned_by?(user)).to be false
      end
    end

    context 'entry test' do
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

      let!(:entry){ create(:entry_other, user: create(:user), challenge_id: challenge.id, score: 10) }
      let!(:entry1){ create(:entry_other, user: create(:user), challenge_id: challenge.id, score: 20) }
      let!(:entry2){ create(:entry_other, user: create(:user), challenge_id: challenge.id, score: 50) }
      it 'return ' do

        puts "------"
        pp EntryOther.all.to_a
        puts "------"
        pp RepositoryEntry.worst_score(challenge.id)
      end
    end
  end

end
