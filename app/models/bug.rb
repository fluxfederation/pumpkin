class Bug < ApplicationRecord
  belongs_to :primary_occurrence, class_name: 'Occurrence'
  has_many :events
  has_many :occurrences

  belongs_to :latest_event, class_name: 'Event', optional: true

  scope :in_occurrence_order, ->{ order('last_occurred_at DESC') }
  scope :with_occurrence_includes, ->{ includes(:primary_occurrence => :environment) }
  scope :with_primary_occurrence_in_environment, ->(environment_ids){ joins(:primary_occurrence).merge(Occurrence.where(:environment_id => environment_ids)) }

  # closed and open rely on latest_even_name from the bug_with_latest_details view
  scope :closed, ->{ where("latest_event_name =?", "closed") }
  scope :open, ->{ where.not("latest_event_name =?", "closed") }

  def self.with_latest_details
    from('bug_with_latest_details AS bugs')
  end
end
