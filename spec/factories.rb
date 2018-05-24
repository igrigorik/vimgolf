FactoryGirl.define do
  factory :challenge do
    title :test
    description :test
    input :a
    output :b
    diff :c
    user
  end

  factory :entry do
    script 'abc'
    created_at Time.now.utc
    user
    score Faker::Number.between(1, 20)
  end

  factory :user, aliases: [:owner] do
    name Faker::Name.name
    nickname Faker::Internet.user_name
    image Faker::Avatar.image
    uid Faker::Number.number(5)
    provider 'Github'
  end
end
