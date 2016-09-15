class AddNotNullToReferences < ActiveRecord::Migration[5.0]
  def up
    change_column :bugs, :primary_occurrence_id, :uuid, null: false
    change_column :occurrences, :patch_id, :uuid, null: false
  end

  def down
    change_column :bugs, :primary_occurrence_id, :uuid
    change_column :occurrences, :patch_id, :uuid
  end
end
