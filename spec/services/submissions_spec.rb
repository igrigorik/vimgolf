require 'spec_helper'

describe Submissions do
  describe '#user_id' do
    let(:golfer) { create(:user) }
    let(:owner) { create(:user) }
    let(:challenge) { create(:challenge, user: owner) }

    context 'user is the challenge owner' do
      let(:submission) { Submissions.new(owner, challenge.urlkey, 1) }
      it 'return true' do
        expect(submission.creator?).to be true
      end
    end

    context 'user is not challenge owner' do
      let(:submission) { Submissions.new(golfer, challenge.urlkey, 1) }
      it 'return true' do
        expect(submission.creator?).to be false
      end
    end
  end
end
