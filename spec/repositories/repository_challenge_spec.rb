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
        expect(result.first).to eq(challenge1)

        result = RepositoryChallenge.paginate_home_page(per_page: 1, page: 2).to_a
        expect(result.length).to eq(1)
        expect(result.first).to eq(challenge2)
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
      let(:first_entry) { build(:entry, user: user, score: 8, created_at: Time.new(2018, 3, 28)) }
      let(:best_entry) { build(:entry, user: user, score: 7, created_at: Time.new(2018, 4, 28)) }
      let(:other_entry) { build(:entry, user: user, score: 11, created_at: Time.new(2018, 4, 30)) }

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

        expect(result.first['score']).to eq(7)
        expect(result.first['created_at']).to eq(Time.new(2018, 4, 28))
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

        expect(result.first['score']).to eq(7)
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

        expect(result.first['score']).to eq(6)
        expect(result.first['user_id']).to eq(best_user.id)
        expect(result.first['created_at']).to eq(Time.new(2019))

        expect(result.second['score']).to eq(7)
        expect(result.second['user_id']).to eq(user2.id)
        expect(result.second['created_at']).to eq(Time.new(2016))

        expect(result.last['score']).to eq(7)
        expect(result.last['user_id']).to eq(user1.id)
        expect(result.last['created_at']).to eq(Time.new(2017))
      end
    end
  end

  describe '.count_uniq_users(challenge_id)' do
    context 'when there is no entries in challenge' do
      let(:challenge) { create(:challenge) }

      it 'return 0' do
        expect(RepositoryChallenge.count_uniq_users(challenge.urlkey)).to eq(0)
      end
    end

    context 'when there is entries in challenge' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge) }
      let(:first_entry) { build(:entry, user: user, score: 8, created_at: Time.new(2018, 3, 28)) }
      let(:other_entry) { build(:entry, user: user, score: 11, created_at: Time.new(2018, 4, 30)) }

      before do
        challenge.entries << first_entry
        challenge.entries << other_entry
      end

      it 'return number of distinct users' do
        result = RepositoryChallenge.count_uniq_users(challenge.urlkey)
        expect(result).to eq(1)
      end
    end
  end

  describe '.worst_score(challenge_id)' do
    context 'when there is no entries in challenge' do
      let(:challenge) { create(:challenge) }

      it 'return nil' do
        expect(RepositoryChallenge.worst_score(challenge.urlkey)).to eq(nil)
      end
    end

    context 'when there is entries in challenge' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge) }

      before do
        challenge.entries <<  build(:entry, user: user, score: 8, created_at: Time.new(2018, 3, 28))
        challenge.entries <<  build(:entry, user: user, score: 11, created_at: Time.new(2018, 4, 30))
      end

      it 'return the worst VISIBLE score (here, 11 will never be visible)' do
        result = RepositoryChallenge.worst_score(challenge.urlkey)
        expect(result).to eq(8)
      end
    end
  end

  describe '.bellow_score(challenge_id, score)' do
    context 'when there is no entries in challenge' do
      let(:challenge) { create(:challenge) }

      it 'return 0' do
        expect(RepositoryChallenge.bellow_score(challenge.urlkey, 100)).to eq(0)
      end
    end

    context 'when there is entries in challenge' do
      let(:userA) { create(:user) }
      let(:userB) { create(:user) }
      let(:userC) { create(:user) }
      let(:challenge) { create(:challenge) }

      before do
        challenge.entries <<  build(:entry, user: userA, score: 1, created_at: Time.new(2018, 3, 28))
        challenge.entries <<  build(:entry, user: userA, score: 2, created_at: Time.new(2018, 4, 30))

        challenge.entries <<  build(:entry, user: userB, score: 2, created_at: Time.new(2018, 3, 28))
        challenge.entries <<  build(:entry, user: userB, score: 9, created_at: Time.new(2018, 4, 30))

        challenge.entries <<  build(:entry, user: userC, score: 10, created_at: Time.new(2018, 3, 28))
      end

      it 'return the next VISIBLE score' do
        result = RepositoryChallenge.bellow_score(challenge.urlkey, 10)
        expect(result).to eq(2)
      end
    end
  end

  describe '.best_player_score(challenge_id, user_id)' do
    context 'when there is no entries in challenge' do
      let(:challenge) { create(:challenge) }
      let(:user) { create(:user) }

      it 'return nil' do
        expect(RepositoryChallenge.best_player_score(challenge.urlkey, user.id)).to eq(nil)
      end
    end

    context 'when there is entries in challenge' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge) }

      before do
        challenge.entries <<  build(:entry, user: user, score: 2, created_at: Time.new(2018, 3, 28))
        challenge.entries <<  build(:entry, user: user, score: 9, created_at: Time.new(2018, 4, 30))
      end

      it 'return the next VISIBLE score' do
        result = RepositoryChallenge.best_player_score(challenge.urlkey, user.id)
        expect(result).to eq(2)
      end
    end
  end

  describe '.count_remaining_solutions(challenge_id, user_id)' do
    context 'when there is no entries in challenge' do
      let(:challenge) { create(:challenge) }

      it 'return 0' do
        expect(RepositoryChallenge.count_remaining_solutions(challenge.urlkey, 40)).to eq(0)
      end
    end

    context 'when there is entries in challenge' do
      let(:user) { create(:user) }
      let(:user2) { create(:user) }
      let(:challenge) { create(:challenge) }

      before do
        challenge.entries <<  build(:entry, user: user, score: 2, created_at: Time.new(2018, 3, 28))
        challenge.entries <<  build(:entry, user: user, score: 9, created_at: Time.new(2018, 4, 30))
        challenge.entries <<  build(:entry, user: user2, score: 10, created_at: Time.new(2018, 4, 30))
      end

      it 'return the sum of VISIBLE solution, here "2" because it is grouped by user' do
        result = RepositoryChallenge.count_remaining_solutions(challenge.urlkey, 11)
        expect(result).to eq(2)
      end
    end
  end

  describe '.count_displayed_solutions(challenge_id, user_id)' do
    context 'when there is no entries in challenge' do
      let(:challenge) { create(:challenge) }

      it 'return 0' do
        expect(RepositoryChallenge.count_displayed_solutions(challenge.urlkey, 0)).to eq(0)
      end
    end

    context 'when there is entries in challenge' do
      let(:user) { create(:user) }
      let(:user2) { create(:user) }
      let(:challenge) { create(:challenge) }

      before do
        challenge.entries <<  build(:entry, user: user, score: 2, created_at: Time.new(2018, 3, 28))
        challenge.entries <<  build(:entry, user: user, score: 9, created_at: Time.new(2018, 4, 30))
        challenge.entries <<  build(:entry, user: user2, score: 10, created_at: Time.new(2018, 4, 30))
      end

      it 'return the sum of VISIBLE solution, here "2" because it is grouped by user' do
        result = RepositoryChallenge.count_displayed_solutions(challenge.urlkey, 2)
        expect(result).to eq(2)
      end
    end
  end

  describe '.player_best_scores(user_id)' do
    context 'when player has not submitted any entries anywhere' do
      let(:user) { create(:user) }
      let(:challenge) { create(:challenge) }

      it 'return empty array' do
        expect(RepositoryChallenge.player_best_scores(user.id).to_a).to eq([])
      end
    end

    # Disable, player_best_score(..., true) not implemented.
    xcontext 'when player has submitted two entries to a challenge' do
      let(:user) { create(:user) }
      let(:user2) { create(:user) }
      let(:challenge1) { create(:challenge) }

      before do
        challenge1.entries <<  build(:entry, user: user, score: 15, created_at: Time.new(2018, 3, 28))
        challenge1.entries <<  build(:entry, user: user, score: 12, created_at: Time.new(2018, 4, 15))
        challenge1.entries <<  build(:entry, user: user2, score: 10, created_at: Time.new(2018, 4, 30))
      end

      it 'return expected user information about the user' do
        result = RepositoryChallenge.player_best_scores(user.id, true).to_a
        expect(result.length).to eq(1)
        challenge = result.first
        expect(challenge['id']).to eq(challenge1.id)
        expect(challenge['title']).to eq(challenge1.title)
        expect(challenge['description']).to eq(challenge1.description)
        expect(challenge['count_entries']).to eq(3)
        expect(challenge['best_score']).to eq(10)
        expect(challenge['best_player_score']).to eq(12)
        expect(challenge['attempts']).to eq(2)
        expect(challenge['position']).to eq(2)
      end
    end

    # Disable, player_best_score(..., true) not implemented.
    xcontext 'when player has submitted entries to multiple challenges' do
      let(:user) { create(:user) }
      let(:user2) { create(:user) }
      let(:challenge1) { create(:challenge, user: user, created_at: Time.new(2017)) }
      let(:challenge2) { create(:challenge, user: user, created_at: Time.new(2018)) }

      before do
        challenge1.entries <<  build(:entry, user: user, score: 15, created_at: Time.new(2018, 3, 28))
        challenge1.entries <<  build(:entry, user: user, score: 12, created_at: Time.new(2018, 4, 15))
        challenge1.entries <<  build(:entry, user: user2, score: 10, created_at: Time.new(2018, 4, 30))
        challenge2.entries <<  build(:entry, user: user, score: 14, created_at: Time.new(2018, 3, 28))
      end

      it 'return expected user information about the user' do
        result = RepositoryChallenge.player_best_scores(user.id, true).to_a
        expect(result.length).to eq(2)

        # Ensure challenges are in chronological order, challenge2 is first.
        challenge = result[0]
        expect(challenge['id']).to eq(challenge2.id)
        expect(challenge['title']).to eq(challenge2.title)
        expect(challenge['description']).to eq(challenge2.description)
        expect(challenge['count_entries']).to eq(1)
        expect(challenge['best_score']).to eq(14)
        expect(challenge['best_player_score']).to eq(14)
        expect(challenge['attempts']).to eq(1)
        expect(challenge['position']).to eq(1)
        expect(challenge['count_golfers']).to eq(1)

        # And challenge1 is next.
        challenge = result[1]
        expect(challenge['id']).to eq(challenge1.id)
        expect(challenge['title']).to eq(challenge1.title)
        expect(challenge['description']).to eq(challenge1.description)
        expect(challenge['count_entries']).to eq(3)
        expect(challenge['best_score']).to eq(10)
        expect(challenge['best_player_score']).to eq(12)
        expect(challenge['attempts']).to eq(2)
        expect(challenge['position']).to eq(2)
        expect(challenge['count_golfers']).to eq(2)
      end
    end
  end

  xdescribe '.submissions_per_player(challenge_id, player_id)' do
    context 'when there are entries in challenge' do
      let(:user1) { create(:user) }
      let(:user2) { create(:user) }
      let(:user3) { create(:user) }
      let(:user4) { create(:user) }
      let(:user5) { create(:user) }
      let(:challenge) { create(:challenge) }

      before do
        challenge.entries <<  build(:entry, user: user1, score: 52, created_at: Time.new(2018, 3, 27))
        challenge.entries <<  build(:entry, user: user5, score: 51, created_at: Time.new(2018, 3, 28))
        challenge.entries <<  build(:entry, user: user2, score: 49, created_at: Time.new(2018, 3, 29))
        challenge.entries <<  build(:entry, user: user3, score: 45, created_at: Time.new(2018, 3, 30))
        challenge.entries <<  build(:entry, user: user1, score: 44, created_at: Time.new(2018, 4, 1))
        challenge.entries <<  build(:entry, user: user4, score: 40, created_at: Time.new(2018, 4, 2))
        challenge.entries <<  build(:entry, user: user2, score: 38, created_at: Time.new(2018, 4, 3))
        challenge.entries <<  build(:entry, user: user2, score: 37, created_at: Time.new(2018, 4, 4))
        challenge.entries <<  build(:entry, user: user3, score: 32, created_at: Time.new(2018, 4, 5))
        challenge.entries <<  build(:entry, user: user1, score: 29, created_at: Time.new(2018, 4, 6))
        challenge.entries <<  build(:entry, user: user4, score: 27, created_at: Time.new(2018, 4, 7))
        challenge.entries <<  build(:entry, user: user3, score: 22, created_at: Time.new(2018, 4, 11))
      end

      it 'returns the three entries by the test user, with appropriate rankings' do
        result = RepositoryChallenge.submissions_per_player(challenge.id, user1.id).to_a
        expect(result.length).to eq(3)

        entry = result[0]
        expect(entry['user_id']).to eq(user1.id)
        expect(entry['score']).to eq(29)
        expect(entry['position']).to eq(3)
        expect(entry['created_at']).to eq(Time.new(2018, 4, 6))

        entry = result[1]
        expect(entry['user_id']).to eq(user1.id)
        expect(entry['score']).to eq(44)
        expect(entry['position']).to eq(5) # >4
        expect(entry['created_at']).to eq(Time.new(2018, 4, 1))

        entry = result[2]
        expect(entry['user_id']).to eq(user1.id)
        expect(entry['score']).to eq(52)
        expect(entry['position']).to eq(7) # >5
        expect(entry['created_at']).to eq(Time.new(2018, 3, 27))
      end
    end
  end
end
