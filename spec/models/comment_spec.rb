require 'spec_helper'

describe Comment do
  let (:comment) { described_class.new }
  describe 'Validations' do
    context 'comment without nickname' do
      it 'return false' do
        comment.comment = 'Foo'

        expect(comment).not_to be_valid
      end
    end

    context 'comment without a comment' do
      it 'return false' do
        comment.nickname = 'Bar'

        expect(comment).not_to be_valid
      end
    end

    context 'comment with comment and nickname' do
      it 'return true' do
        comment.comment = 'Foo'
        comment.nickname = 'Bar'

        expect(comment).to be_valid
      end
    end
  end
end
