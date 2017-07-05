class OptimiseBugSummaries < ActiveRecord::Migration[5.0]
  def up
    execute <<-end_sql
CREATE OR REPLACE VIEW bug_summaries AS (
        WITH latest_occurrences AS (
         SELECT DISTINCT ON (occurrences.bug_id) occurrences.bug_id, occurrences.occurred_at
           FROM occurrences
           ORDER BY occurrences.bug_id, occurrences.occurred_at DESC
        ), latest_events AS (
         SELECT DISTINCT ON (events.bug_id) events.id,
            events.bug_id,
            events.name,
            events.created_at,
            events.updated_at
           FROM events
          ORDER BY events.bug_id, events.created_at DESC
        )
 SELECT bugs.id,
    primary_occurrence.message,
    latest_events.name AS latest_event_name,
    latest_events.created_at AS latest_event_at,
    primary_occurrence.occurred_at AS first_occurred_at,
    (SELECT MAX(occurred_at) FROM occurrences WHERE bug_id = bugs.id) AS last_occurred_at,
    CASE
      WHEN latest_events.name::text = 'closed'::text THEN latest_events.created_at
      ELSE NULL::timestamp without time zone
    END AS closed_at,
    (SELECT count(1) AS count
       FROM occurrences
      WHERE occurrences.bug_id = bugs.id) AS occurrence_count
   FROM bugs
   JOIN latest_events ON bugs.id = latest_events.bug_id
   JOIN occurrences primary_occurrence ON bugs.primary_occurrence_id = primary_occurrence.id
);
    end_sql

    # This compound index is key to getting reasonable performance
    add_index :occurrences, [:bug_id, :occurred_at]
    remove_index :occurrences, :occurred_at
    remove_index :occurrences, :bug_id
  end

  def down
    remove_index :occurrences, [:bug_id, :occurred_at]
    add_index :occurrences, :occurred_at
    add_index :occurrences, :bug_id

    execute <<-end_sql
CREATE OR REPLACE VIEW bug_summaries AS (
  WITH
  latest_occurrences AS (
    SELECT DISTINCT ON (bug_id) *
      FROM occurrences
     ORDER BY bug_id, occurred_at DESC
  ),
  latest_events AS (
    SELECT DISTINCT ON (bug_id) *
      FROM events
     ORDER BY bug_id, created_at DESC
  )
  SELECT bugs.id,
         primary_occurrence.message,
         latest_events.name AS latest_event_name,
         latest_events.created_at AS latest_event_at,
         primary_occurrence.occurred_at AS first_occurred_at,
         latest_occurrences.occurred_at AS last_occurred_at,
         (CASE WHEN latest_events.name = 'closed' THEN latest_events.created_at ELSE NULL END) AS closed_at,
         (SELECT COUNT(1) FROM occurrences WHERE bug_id = bugs.id) AS occurrence_count
    FROM bugs
    JOIN latest_events
      ON bugs.id = latest_events.bug_id
    JOIN latest_occurrences
      ON bugs.id = latest_occurrences.bug_id
    JOIN occurrences AS primary_occurrence
      ON bugs.primary_occurrence_id = primary_occurrence.id
);
    end_sql
  end
end
