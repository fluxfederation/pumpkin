class CreateIssues < ActiveRecord::Migration[5.0]
  def change
    enable_extension 'pgcrypto' unless extension_enabled?('pgcrypto')
    create_table :issues, id: :uuid, default: 'gen_random_uuid()' do |t|
      t.references :bug, type: :uuid, null: false
      t.string :url, null: false

      t.timestamps
    end
  end
end
