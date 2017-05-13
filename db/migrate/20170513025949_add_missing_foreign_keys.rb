class AddMissingForeignKeys < ActiveRecord::Migration[5.0]
  def up
    execute "ALTER TABLE occurrences ADD FOREIGN KEY (bug_id) REFERENCES bugs (id)"
    execute "ALTER TABLE events ADD FOREIGN KEY (bug_id) REFERENCES bugs (id)"
    execute "ALTER TABLE bugs ADD FOREIGN KEY (primary_occurrence_id) REFERENCES occurrences (id)"
  end

  def down
    execute "ALTER TABLE occurrences DROP CONSTRAINT occurrences_bug_id_fkey"
    execute "ALTER TABLE events DROP CONSTRAINT events_bug_id_fkey"
    execute "ALTER TABLE bugs DROP CONSTRAINT bugs_primary_occurrence_id_fkey"
  end
end
