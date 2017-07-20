class BetterOccurrenceIndices < ActiveRecord::Migration[5.0]
  def up
    execute "DROP INDEX index_occurrences_on_environment_id"
    execute "CREATE INDEX index_occurrences_on_environment_id_and_bug_id ON occurrences (environment_id, bug_id)"
  end
  def down
    execute "DROP INDEX index_occurrences_on_environment_id_and_bug_id"
    execute "CREATE INDEX index_occurrences_on_environment_id ON occurrences (environment_id)"
  end
end
