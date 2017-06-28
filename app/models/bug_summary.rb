class BugSummary < ApplicationRecord
  # backed by a view
  self.primary_key = :id
  def readonly?; true end

  belongs_to :bug, foreign_key: :id
  has_many :issues, through: :bug
  has_many :occurrences, through: :bug
  has_one :primary_occurrence, through: :bug

  scope :in_occurrence_order, ->{ order('last_occurred_at DESC') }
  scope :closed, ->{ where("closed_at IS NOT NULL") }
  scope :open, ->{ where("closed_at IS NULL") }

  scope :not_newer_than_bug, ->(bug_id) {
    # TODO: nicer way to write this?
    where("last_occurred_at <= (SELECT last_occurred_at FROM #{table_name} b2 WHERE b2.id = ?)", bug_id)
  }
end
