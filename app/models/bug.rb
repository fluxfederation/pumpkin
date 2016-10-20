class Bug < ApplicationRecord
  belongs_to :primary_occurrence, class_name: 'Occurrence'
  has_many :events
  has_many :occurrences

  belongs_to :latest_event, class_name: 'Event', optional: true

  def self.with_latest_details
    select('DISTINCT ON (bugs.id) bugs.*, ev_latest.id AS latest_event_id, occ_latest.occurred_at AS last_occurred_at')
    .from('bugs INNER JOIN occurrences occ_latest ON bugs.id = occ_latest.bug_id INNER JOIN events ev_latest ON bugs.id = ev_latest.bug_id')
    .order('bugs.id, occ_latest.occurred_at DESC, ev_latest.created_at DESC')
  end
end
