require 'spec_helper'

describe Comment do
  it { is_expected.to validate_presence_of(:nickname) }
  it { is_expected.to validate_presence_of(:comment) }
  it { is_expected.to validate_length_of(:comment) }
end
