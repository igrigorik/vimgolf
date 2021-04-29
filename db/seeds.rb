# Run this way :
# bundle exec rake db:setup
# or
# bundle exec rake db:setup challenges=400 users=30 entries=100

params = {
  # create n challenges
  challenges: 0,
  # create n users, they create challenge or entries
  users: 0,
  # create n entries PER challenges
  entries: 0
}.merge(ARGV[1..].map { |param| param.split('=') }.map { |k, v| [k.to_sym, v.to_i] }.to_h)

users = []

params[:users].times do |i|
  users << User.create(
    name: Faker::Name.name,
    nickname: "#{Faker::Internet.user_name}_#{i}",
    image: Faker::Avatar.image,
    uid: Faker::Number.number(digits: 5),
    provider: 'Github',
    created_at: Time.now,
    updated_at: Time.now
  )
end

params[:challenges].times do |i|
  challenge = Challenge.create(
    title: Faker::Book.title,
    description: "description #{i}",
    diff: 'diff',
    input: 'input',
    input_type: 'input_type',
    output: 'output',
    output_type: 'output_type',
    user: users.sample,
    created_at: Time.now,
    updated_at: Time.now
  )

  params[:entries].times do |j|
    challenge.entries.create(
      script: "a\ra\na\r\nentries: #{j}\n\r\n",
      created_at: Time.now,
      updated_at: Time.now,
      user: users.sample,
      score: Faker::Number.between(from: 2, to: 200)
    )
  end
end
