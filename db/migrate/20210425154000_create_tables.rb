class CreateTables < ActiveRecord::Migration[5.0]
  def change
    create_table :users do |t|
      t.string :provider
      t.bigint :uid
      t.string :nickname, null: false, index: true
      t.string :name
      t.string :location
      t.string :image
      t.string :description
      t.string :key, null: false, index: true

      t.timestamps null: false
    end
    create_table :challenges do |t|
      t.string :title, null: false
      t.text :description
      t.text :diff
      t.text :input, null: false
      t.string :input_type, null: false
      t.text :output, null: false
      t.string :output_type, null: false
      t.string :legacy_urlkey, limit: 24, null: true, index: { unique: true }
      t.references :user, null: false, foreign_key: true

      t.timestamps null: false

      t.index :created_at
    end
    create_table :entries do |t|
      t.binary :script, null: false
      t.integer :score, null: false
      t.references :user, null: false, foreign_key: true
      t.references :challenge, null: false, foreign_key: true

      t.timestamps null: false

      t.index [:challenge_id, :score, :created_at]
    end
    create_table :comments do |t|
      t.string :comment, null: false
      t.references :user, null: false, foreign_key: true
      t.references :entry, null: false, foreign_key: { to_table: :entries }

      t.timestamps null: false
    end
  end
end
