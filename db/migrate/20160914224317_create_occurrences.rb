class CreateOccurrences < ActiveRecord::Migration[5.0]
  def change
    enable_extension 'pgcrypto' unless extension_enabled?('pgcrypto')
    create_table :occurrences, id: :uuid, default: 'gen_random_uuid()' do |t|
      t.string 'message'
      t.datetime 'occurred_at', null: false
      t.json 'data', null: false

      t.timestamps
    end
  end
end
