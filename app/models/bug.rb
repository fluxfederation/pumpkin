class Bug < ApplicationRecord
  belongs_to :primary_occurrence, class_name: 'Occurrence'
  has_many :events
  has_many :occurrences

  belongs_to :latest_event, class_name: 'Event', optional: true

  def self.with_latest_details
    from('bug_with_latest_details AS bugs')
  end
end
