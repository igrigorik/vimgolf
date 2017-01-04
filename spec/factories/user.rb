FactoryGirl.define do
  factory :user, aliases: [:owner] do
    name Faker::Name.name
    nickname Faker::Internet.user_name
    image Faker::Avatar.image
    uid Faker::Number.number(5)
    provider 'Github'
  end
end
