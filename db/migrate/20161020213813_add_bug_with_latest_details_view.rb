class AddBugWithLatestDetailsView < ActiveRecord::Migration[5.0]
  def up
    command = <<-SQL
      CREATE VIEW bug_with_latest_details AS
      SELECT DISTINCT ON (bugs.id) bugs.*, ev_latest.id AS latest_event_id, ev_latest.name AS latest_event_name, occ_latest.occurred_at AS last_occurred_at
        FROM bugs
      INNER JOIN occurrences occ_latest ON bugs.id = occ_latest.bug_id
      INNER JOIN events ev_latest ON bugs.id = ev_latest.bug_id
      ORDER BY bugs.id, occ_latest.occurred_at DESC, ev_latest.created_at DESC;
    SQL

    Bug.connection.execute(command)
  end

  def down
    Bug.connection.execute('DROP VIEW bug_with_latest_details')
  end
end
