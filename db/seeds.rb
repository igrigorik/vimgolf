# Run this way :
# bundle exec rake db:setup
# or
# bundle exec rake db:setup challenges=400 users=30 entries=100

DatabaseHelper.create_collections

params = {
  # create n challenges
  challenges: 0,
  # create n users, they create challenge or entries
  users: 0,
  # create n entries PER challenges
  entries: 0,
}.merge(ARGV[1..-1].map{|param| param.split('=')}.map{|k, v| [k.to_sym, v.to_i]}.to_h)

users = []

params[:users].times do |i|
  users.push(User.new(
    name: Faker::Name.name,
    nickname: "#{Faker::Internet.user_name}_#{i}",
    image: Faker::Avatar.image,
    uid: Faker::Number.number(digits: 5),
    provider: 'Github',
    created_at: Time.now,
    updated_at: Time.now,
  ).as_document)
end
User.collection.insert_many(users)

user_ids = User.pluck(:_id)

params[:challenges].times do |i|
  entries = []
  params[:entries].times do |j|
    entries.push(Entry.new(
      script: "a\ra\na\r\nentries: #{j}\n\r\n",
      created_at: Time.now,
      updated_at: Time.now,
      user_id: user_ids.sample,
      score: Faker::Number.between(from: 2, to: 200)
    ).as_document)
  end

  Challenge.collection.insert_one(Challenge.new(
    title: Faker::Book.title,
    description: "description #{i}",
    diff: 'diff',
    input: 'input',
    input_type: 'input_type',
    output: 'output',
    output_type: 'output_type',
    user_id: user_ids.sample,
    created_at: Time.now,
    updated_at: Time.now,
    entries: entries,
  ).as_document)
end
