require 'spec_helper'

describe Comment do
  let(:user) { build(:user) }
  let(:entry) { build(:entry) }
  let(:comment) { described_class.new(user: user, entry: entry) }

  describe 'Validations' do
    context 'comment without a comment' do
      it 'return false' do
        expect(comment).not_to be_valid
      end
    end

    context 'comment with comment' do
      it 'return true' do
        comment.comment = 'Foo'
        expect(comment).to be_valid
      end
    end
  end
end
