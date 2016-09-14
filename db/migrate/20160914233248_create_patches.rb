class CreatePatches < ActiveRecord::Migration[5.0]
  def change
    enable_extension 'pgcrypto' unless extension_enabled?('pgcrypto')
    create_table :patches, id: :uuid, default: 'gen_random_uuid()' do |t|
      t.text :name
      t.index :name, unique: true
      t.timestamps
    end
  end
end
