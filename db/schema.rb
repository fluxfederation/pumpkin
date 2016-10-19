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

ActiveRecord::Schema.define(version: 20161019211306) do

  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"
  enable_extension "pgcrypto"

  create_table "bugs", id: :uuid, default: -> { "gen_random_uuid()" }, force: :cascade do |t|
    t.uuid     "primary_occurrence_id", null: false
    t.datetime "created_at",            null: false
    t.datetime "updated_at",            null: false
    t.index ["primary_occurrence_id"], name: "index_bugs_on_primary_occurrence_id", using: :btree
  end

  create_table "events", id: :uuid, default: -> { "gen_random_uuid()" }, force: :cascade do |t|
    t.integer  "bug_id",     null: false
    t.string   "name",       null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["bug_id"], name: "index_events_on_bug_id", using: :btree
  end

  create_table "occurrences", id: :uuid, default: -> { "gen_random_uuid()" }, force: :cascade do |t|
    t.string   "message"
    t.datetime "occurred_at", null: false
    t.json     "data",        null: false
    t.datetime "created_at",  null: false
    t.datetime "updated_at",  null: false
    t.uuid     "patch_id",    null: false
    t.uuid     "bug_id"
    t.index ["bug_id"], name: "index_occurrences_on_bug_id", using: :btree
    t.index ["patch_id"], name: "index_occurrences_on_patch_id", using: :btree
  end

  create_table "patches", id: :uuid, default: -> { "gen_random_uuid()" }, force: :cascade do |t|
    t.text     "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["name"], name: "index_patches_on_name", unique: true, using: :btree
  end

end
