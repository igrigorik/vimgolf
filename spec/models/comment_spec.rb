require 'spec_helper'

describe Comment do
  let(:comment) { described_class.new }

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
