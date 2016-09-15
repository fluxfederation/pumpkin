class CreateBugs < ActiveRecord::Migration[5.0]
  def change
    enable_extension 'pgcrypto' unless extension_enabled?('pgcrypto')
    create_table :bugs, id: :uuid, default: 'gen_random_uuid()' do |t|
      t.references :primary_occurrence, type: :uuid

      t.timestamps
    end
  end
end
