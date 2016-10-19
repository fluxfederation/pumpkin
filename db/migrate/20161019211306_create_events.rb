class CreateEvents < ActiveRecord::Migration[5.0]
  def change
    enable_extension 'pgcrypto' unless extension_enabled?('pgcrypto')
    create_table :events, id: :uuid, default: 'gen_random_uuid()' do |t|
      t.references :bug, type: :uuid, null: false
      t.string :name, null: false

      t.timestamps
    end
  end
end
