class Bug < ApplicationRecord
  belongs_to :primary_occurrence, class_name: 'Occurrence'
  has_many :events
  has_many :occurrences

  belongs_to :latest_event, class_name: 'Event', optional: true

  def self.with_latest_details
    select("bugs.*, e1.id AS latest_event_id, latest_occurrences.occurred_at AS last_occurred_at").
      joins("JOIN events e1 ON bugs.id = e1.bug_id").
      joins("LEFT OUTER JOIN events e2 ON bugs.id = e2.bug_id AND e1.created_at < e2.created_at").
      where("e2.id IS NULL").
      from("(#{Occurrence.order("occurred_at DESC").limit(1).to_sql}) latest_occurrences, bugs")
  end
end
