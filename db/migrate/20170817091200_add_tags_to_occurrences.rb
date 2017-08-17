class AddTagsToOccurrences < ActiveRecord::Migration[5.0]
  def up
    add_column :occurrences, :tags, :string, array: true, null: false, default: []
    execute 'CREATE INDEX index_occurrences_on_tags on occurrences USING GIN ("tags")'
  end

  def down
    remove_index :occurrences, name: "index_occurrences_on_tags"
    remove_column :occurrences, :tags
  end
end

