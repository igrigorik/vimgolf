require 'spec_helper'

describe Challenge do

  describe 'Validations' do
    it { is_expected.to have_fields(:title, :description, :input, :output, :diff) }

    it { is_expected.to validate_presence_of(:title) }
    it { is_expected.to validate_presence_of(:description) }

    it { is_expected.to validate_length_of(:input) }
    it { is_expected.to validate_length_of(:output) }
    it { is_expected.to validate_length_of(:diff) }
  end

  describe '#participator?' do
    let(:owner) { create(:user) }
    let(:challenge) { create(:challenge, user: owner) }

    context 'user is the challenge owner' do
      it 'return true' do
        expect(challenge.participator?(owner)).to be true
      end
    end

    context 'when user has an entry in challenge' do
      let(:participant) { create(:user) }

      before do
        challenge.entries << build(:entry, user: participant)
      end

      it 'return true' do
        expect(challenge.participator?(participant)).to be true
      end
    end

    context 'when user is admin' do
      let(:admin) { create(:user, nickname: 'igrigorik') }

      it 'return true' do
        expect(challenge.participator?(admin)).to be true
      end
    end
  end

  describe '#top_entries' do
    context 'when user has more than one entry' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge, user: user) }
      let(:participant) { create(:user) }
      let(:best_entry) { build(:entry, user: participant, score: 1) }
      let(:another_entry) { build(:entry, user: user, score: 1) }

      before do
        challenge.entries << build(:entry, user: participant, score: 10)
        challenge.entries << best_entry
        challenge.entries << another_entry
      end

      it 'return most recent entry with best score per user' do
        expect(challenge.top_entries).to match_array [another_entry, best_entry]
      end
    end
  end

  describe '#allowed_entries' do
    let(:user) { create(:user) }
    let(:challenge) { create(:challenge, user: user) }
    let(:participant) { create(:user) }
    let(:another_participant) { create(:user) }
    let(:non_competitor) { create(:user) }
    let(:best_entry) { build(:entry, user: participant, score: 1) }
    let(:another_entry) { build(:entry, user: user, score: 1) }
    let(:almost_good_entry) { build(:entry, user: participant, score: 10) }
    let(:not_so_good_entry) { build(:entry, user: another_participant, score: 20) }

    before do
      challenge.entries << almost_good_entry
      challenge.entries << best_entry
      challenge.entries << another_entry
      challenge.entries << not_so_good_entry
    end

    context 'when user is admin' do
      it 'can see all top entries' do
        allowed_entries = challenge.allowed_entries(user)

        expect(allowed_entries[0]).to match_array [another_entry, best_entry, not_so_good_entry]
        expect(allowed_entries[1]).to eq 0
      end
    end

    context 'when user is a competitor' do
      it 'can see all entries below her' do
        allowed_entries = challenge.allowed_entries(another_participant)

        expect(allowed_entries[0]).to match_array [another_entry, best_entry, not_so_good_entry]
        expect(allowed_entries[1]).to eq 0
      end
    end

    context 'when user is not a competitor' do
      it 'can see all entries bottom 20%' do
        allowed_entries = challenge.allowed_entries(non_competitor)

        expect(allowed_entries[0]).to match_array [not_so_good_entry]
        expect(allowed_entries[1]).to eq 2
      end
    end
  end
end
