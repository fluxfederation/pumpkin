class Bug < ApplicationRecord
  belongs_to :primary_occurrence, class_name: 'Occurrence'
  has_many :events
  has_many :occurrences

  belongs_to :latest_event, class_name: 'Event', optional: true

  def self.with_latest_event
    select("bugs.*, e1.id AS latest_event_id").
      joins("JOIN events e1 ON bugs.id = e1.bug_id").
      joins("LEFT OUTER JOIN events e2 ON bugs.id = e2.bug_id AND e1.created_at < e2.created_at").
      where("e2.id IS NULL")
  end
end
