FactoryGirl.define do
  factory :entry do
    script :a
    created_at Time.now
    user
    score Faker::Number.between(1, 20)
  end
end
