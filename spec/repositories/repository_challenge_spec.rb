require 'spec_helper'

describe RepositoryChallenge do

  describe '.count_entries' do
    context 'when there is nothing' do
      it 'return 0' do
        expect(RepositoryChallenge.count_entries).to eq(0)
      end
    end

    context 'when there is some entries' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge, user: user) }

      before do
        challenge.entries << build(:entry, user: user, score: 10)
        challenge.entries << build(:entry, user: user, score: 9)
        challenge.entries << build(:entry, user: user, score: 8)
      end

      it 'return number of entries' do
        expect(RepositoryChallenge.count_entries).to eq(3)
      end
    end
  end

  describe '.paginate_home_page' do
    context 'when there is nothing' do
      it 'return empty array' do
        expect(RepositoryChallenge.paginate_home_page(per_page: 100, page: 1).to_a).to eq([])
      end
    end

    context 'when there is some entries' do
      let(:user) { create(:user) }
      let(:challenge1) { create(:challenge, user: user, created_at: Time.new(2018)) }
      let(:challenge2) { create(:challenge, user: user, created_at: Time.new(2017)) }

      before do
        challenge1.entries << build(:entry, user: user, score: 10)
        challenge1.entries << build(:entry, user: user, score: 9)
        challenge2.entries << build(:entry, user: user, score: 8)
      end

      it 'return paginated number of entries' do
        result = RepositoryChallenge.paginate_home_page(per_page: 1, page: 1).to_a
        expect(result.length).to eq(1)
        challenge = result.first
        expect(challenge['_id']).to eq(challenge1.id)
        expect(challenge['count_entries']).to eq(2)
        expect(challenge['score']).to be_within(1).of(0.1)

        result = RepositoryChallenge.paginate_home_page(per_page: 1, page: 2).to_a
        expect(result.length).to eq(1)
        challenge = result.first
        expect(challenge['_id']).to eq(challenge2.id)
        expect(challenge['count_entries']).to eq(1)
        expect(challenge['score']).to be_within(1).of(0.1)
      end
    end
  end

end
