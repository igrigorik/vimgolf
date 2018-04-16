# Run this way :
# bundle exec rake db:setup
# or
# bundle exec rake db:setup challenges=400 users=30 entries=100

DatabaseHelper.create_collections

params = {
  # Total number of challenges
  challenges: 0,
  # Total number of users, they create challene or entries
  users: 0,
  # Number of entries PER challenges
  entries: 0,
}.merge(ARGV[1..-1].map{|param| param.split('=')}.map{|k, v| [k.to_sym, v.to_i]}.to_h)

users = []

params[:users].times do |i|
  users.push(User.create!(
    name: Faker::Name.name,
    nickname: "#{Faker::Internet.user_name}_#{i}",
    image: Faker::Avatar.image,
    uid: Faker::Number.number(5),
    provider: 'Github',
  ))
end

params[:challenges].times do |i|
  c = Challenge.create!(
    title: Faker::Book.title,
    description: "description #{i}",
    diff: 'diff',
    input: 'input',
    input_type: 'input_type',
    output: 'output',
    output_type: 'output_type',
    user: users.sample,
  )

  params[:entries].times do |j|
    c.entries.create(
      script: "a\ra\na\r\nentries: #{j}\n\r\n",
      created_at: Time.now.utc,
      user: users.sample,
      score: Faker::Number.between(2, 200)
    )
  end
end
