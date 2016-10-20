class Bug < ApplicationRecord
  belongs_to :primary_occurrence, class_name: 'Occurrence'
  has_many :events
  has_many :occurrences

  belongs_to :latest_event, class_name: 'Event', optional: true

  def self.with_latest_details
    select('DISTINCT ON (bugs.id) bugs.*, events.id AS latest_event_id, occ_latest.occurred_at AS last_occurred_at')
    .from('bugs INNER JOIN occurrences occ_latest ON bugs.id = occ_latest.bug_id INNER JOIN events ON bugs.id = events.bug_id')
    .order('bugs.id, occ_latest.occurred_at DESC, events.created_at DESC')
  end
end
