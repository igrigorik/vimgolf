# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 2021_04_25_154000) do

  create_table "challenges", force: :cascade do |t|
    t.string "title", null: false
    t.text "description"
    t.text "diff"
    t.text "input", null: false
    t.string "input_type", null: false
    t.text "output", null: false
    t.string "output_type", null: false
    t.string "legacy_urlkey", limit: 24
    t.integer "user_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["created_at"], name: "index_challenges_on_created_at"
    t.index ["legacy_urlkey"], name: "index_challenges_on_legacy_urlkey", unique: true
    t.index ["user_id"], name: "index_challenges_on_user_id"
  end

  create_table "comments", force: :cascade do |t|
    t.string "comment", null: false
    t.integer "user_id", null: false
    t.integer "entry_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["entry_id"], name: "index_comments_on_entry_id"
    t.index ["user_id"], name: "index_comments_on_user_id"
  end

  create_table "entries", force: :cascade do |t|
    t.binary "script", null: false
    t.integer "score", null: false
    t.integer "user_id", null: false
    t.integer "challenge_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["challenge_id", "score", "created_at"], name: "index_entries_on_challenge_id_and_score_and_created_at"
    t.index ["challenge_id"], name: "index_entries_on_challenge_id"
    t.index ["user_id"], name: "index_entries_on_user_id"
  end

  create_table "users", force: :cascade do |t|
    t.string "provider"
    t.bigint "uid"
    t.string "nickname", null: false
    t.string "name"
    t.string "location"
    t.string "image"
    t.string "description"
    t.string "key", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["key"], name: "index_users_on_key"
    t.index ["nickname"], name: "index_users_on_nickname"
  end

end
