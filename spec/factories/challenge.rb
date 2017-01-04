FactoryGirl.define do
  factory :challenge do
    title :test
    description :test
    input :a
    output :b
    diff :c
    user
  end
end
