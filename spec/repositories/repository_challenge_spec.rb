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

  describe '.paginate_leaderboard(challenge_id)' do
    context 'when there is no entries in challenge' do
      let(:challenge) { create(:challenge) }

      it 'return empty array' do
        expect(RepositoryChallenge.paginate_leaderboard(
          challenge_id: challenge.id,
          per_page: 1000,
          page: 1
        ).to_a).to eq([])
      end
    end

    context 'when there is entries in challenge' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge) }
      let(:first_entry) { build(:entry, user: user, score: 8, created_at: Time.new(2018,03,28)) }
      let(:best_entry) { build(:entry, user: user, score: 7, created_at: Time.new(2018,04,28)) }
      let(:other_entry) { build(:entry, user: user, score: 11, created_at: Time.new(2018,04,30)) }

      before do
        challenge.entries << first_entry
        challenge.entries << best_entry
        challenge.entries << other_entry
      end

      it 'return one score' do
        result = RepositoryChallenge.paginate_leaderboard(
          challenge_id: challenge.id,
          per_page: 1000,
          page: 1
        ).to_a

        expect(result.length).to eq(1)

        expect(result.first['min_score']).to eq(7)
        expect(result.first['created_at']).to eq(Time.new(2018,04,28))
      end
    end

    context 'when there is entries with same score' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge) }
      let(:entry1) { build(:entry, user: user, score: 7, created_at: Time.new(2017)) }
      let(:entry2) { build(:entry, user: user, score: 7, created_at: Time.new(2018)) }

      before do
        challenge.entries << entry2
        challenge.entries << entry1
      end

      it 'return the first created_at score' do
        result = RepositoryChallenge.paginate_leaderboard(
          challenge_id: challenge.id,
          per_page: 1000,
          page: 1
        ).to_a
        expect(result.length).to eq(1)

        expect(result.first['min_score']).to eq(7)
        expect(result.first['created_at']).to eq(Time.new(2017))
      end
    end

    context 'when there is entries with same score but different users' do
      let(:user1) { create(:user) }
      let(:user2) { create(:user) }
      let(:best_user) { create(:user) }
      let(:challenge) { create(:challenge) }

      let(:entry1_user1) { build(:entry, user: user1, score: 7, created_at: Time.new(2017)) }
      let(:entry2_user1) { build(:entry, user: user1, score: 7, created_at: Time.new(2018)) }

      let(:entry1_user2) { build(:entry, user: user2, score: 7, created_at: Time.new(2017)) }
      let(:entry2_user2) { build(:entry, user: user2, score: 7, created_at: Time.new(2016)) }

      let(:best_entry) { build(:entry, user: best_user, score: 6, created_at: Time.new(2019)) }

      before do
        challenge.entries << entry1_user1
        challenge.entries << entry2_user1
        challenge.entries << entry1_user2
        challenge.entries << entry2_user2
        challenge.entries << best_entry
      end

      it 'return the first lowest created_at score of the right user' do
        result = RepositoryChallenge.paginate_leaderboard(
          challenge_id: challenge.id,
          per_page: 1000,
          page: 1
        ).to_a

        expect(result.length).to eq(3)

        expect(result.first['min_score']).to eq(6)
        expect(result.first['user_id']).to eq(best_user.id)
        expect(result.first['created_at']).to eq(Time.new(2019))

        expect(result.second['min_score']).to eq(7)
        expect(result.second['user_id']).to eq(user2.id)
        expect(result.second['created_at']).to eq(Time.new(2016))

        expect(result.last['min_score']).to eq(7)
        expect(result.last['user_id']).to eq(user1.id)
        expect(result.last['created_at']).to eq(Time.new(2017))
      end
    end
  end

  describe '.uniq_users(challenge_id)' do
    context 'when there is no entries in challenge' do
      let(:challenge) { create(:challenge) }

      it 'return empty array' do
        expect(RepositoryChallenge.uniq_users(challenge.id).to_a).to eq([])
      end
    end

    context 'when there is entries in challenge' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge) }
      let(:first_entry) { build(:entry, user: user, score: 8, created_at: Time.new(2018,03,28)) }
      let(:other_entry) { build(:entry, user: user, score: 11, created_at: Time.new(2018,04,30)) }

      before do
        challenge.entries << first_entry
        challenge.entries << other_entry
      end

      it 'return one score' do
        result = RepositoryChallenge.uniq_users(challenge.id).to_a
        expect(result.length).to eq(1)
        expect(result.first[:_id]).to eq(user.id)
      end
    end

  end

end
