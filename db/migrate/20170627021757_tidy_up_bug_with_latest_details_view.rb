class TidyUpBugWithLatestDetailsView < ActiveRecord::Migration[5.0]
  def up
    execute <<-end_sql
CREATE OR REPLACE VIEW bug_with_latest_details AS (
  WITH
  latest_occurrences AS (
    SELECT DISTINCT ON (bug_id) * FROM occurrences ORDER BY bug_id, occurred_at DESC
  ),
  latest_events AS (
    SELECT DISTINCT ON (bug_id) * FROM events ORDER BY bug_id, created_at DESC
  )
  SELECT bugs.id,
     bugs.primary_occurrence_id,
     bugs.created_at,
     bugs.updated_at,
     latest_events.id AS latest_event_id,
     latest_events.name AS latest_event_name,
     latest_occurrences.occurred_at AS last_occurred_at
    FROM bugs
     JOIN latest_events ON bugs.id = latest_events.bug_id
     JOIN latest_occurrences ON bugs.id = latest_occurrences.bug_id
)
;
    end_sql
  end

  def down
    execute <<-end_sql
CREATE OR REPLACE VIEW bug_with_latest_details AS (
SELECT DISTINCT ON (bugs.id) bugs.id,
    bugs.primary_occurrence_id,
    bugs.created_at,
    bugs.updated_at,
    ev_latest.id AS latest_event_id,
    ev_latest.name AS latest_event_name,
    occ_latest.occurred_at AS last_occurred_at
   FROM bugs
     JOIN occurrences occ_latest ON bugs.id = occ_latest.bug_id
     JOIN events ev_latest ON bugs.id = ev_latest.bug_id
 ORDER BY bugs.id, occ_latest.occurred_at DESC, ev_latest.created_at DESC
)
;
    end_sql
  end
end
