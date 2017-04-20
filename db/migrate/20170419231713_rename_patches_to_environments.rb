class RenamePatchesToEnvironments < ActiveRecord::Migration[5.0]
  def up
    remove_index :patches, :name
    rename_table :patches, :environments
    add_index :environments, :name

    rename_column :occurrences, :patch_id, :environment_id
  end

  def down
    remove_index :environments, :name
    rename_table :environments, :patches
    add_index :patches, :name

    rename_column :occurrences, :environment_id, :patch_id
  end
end
