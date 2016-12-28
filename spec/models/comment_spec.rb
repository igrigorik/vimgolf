require 'spec_helper'

describe Comment do
  it { should validate_presence_of(:nickname) }
  it { should validate_presence_of(:comment) }
  it { should validate_length_of(:comment) }
end
