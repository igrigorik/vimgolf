require 'spec_helper'

describe SubmissionsPerUser do
  describe 'All submissions by testuser' do
    context 'multiple entries' do
      let(:user1) { create(:user) }
      let(:user2) { create(:user) }
      let(:user3) { create(:user) }
      let(:user4) { create(:user) }
      let(:user5) { create(:user) }
      let(:challenge) { create(:challenge, user: user3) }

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

      it 'shows all submission by the user itself' do
        result = []
        SubmissionsPerUser.new(user1, challenge.urlkey, user1).each { |e| result.push(e) }

        entry = result[0]
        expect(entry.user).to eq(user1)
        expect(entry.score).to eq(29)
        expect(entry.position).to eq(3)

        entry = result[1]
        expect(entry.user).to eq(user1)
        expect(entry.score).to eq(44)
        expect(entry.position).to eq(">4")

        entry = result[2]
        expect(entry.user).to eq(user1)
        expect(entry.score).to eq(52)
        expect(entry.position).to eq(">5")
      end

      it 'only show submissions if you have enough score' do
        result = []
        SubmissionsPerUser.new(user5, challenge.urlkey, user1).each { |e| result.push(e) }

        # Best score for user5 is 51, so solutions with scores 44 and 29 are hidden.
        entry = result[0]
        expect(entry.user).to eq(user1)
        expect(entry.score).to eq(52)
        expect(entry.position).to eq(">5")
      end

      it 'shows correct number of remaining entries' do
        spu = SubmissionsPerUser.new(user5, challenge.urlkey, user1)

        # Best score for user5 is 51, so solutions with scores 44 and 29 are hidden.
        # That means there are 2 remaining solutions to unlock.
        expect(spu.count_remaining).to eq(2)
      end

      it 'only includes the player in users mapping' do
        spu = SubmissionsPerUser.new(user5, challenge.urlkey, user1)
        expect(spu.users).to eq({ user1.id => user1 })
      end

      it 'shows the correct owner for a challenge' do
        spu = SubmissionsPerUser.new(user5, challenge.urlkey, user1)
        expect(spu.user_id).to eq(user3.id)
      end

      it 'should not be empty' do
        spu = SubmissionsPerUser.new(user5, challenge.urlkey, user1)
        expect(spu.empty?).to eq(false)
      end

      it 'should not highlight the owner' do
        spu = SubmissionsPerUser.new(user5, challenge.urlkey, user1)
        expect(spu.highlight_owner?).to eq(false)
      end

      it 'should not allow player to edit' do
        spu = SubmissionsPerUser.new(user5, challenge.urlkey, user1)
        result = []
        spu.each { |e| result << e }
        expect(spu.player_can_edit?(result[0])).to eq(false)
      end

      it 'counts users correctly' do
        spu = SubmissionsPerUser.new(user5, challenge.urlkey, user1)
        expect(spu.count_uniq_users).to eq(5)
      end
    end
  end
end
