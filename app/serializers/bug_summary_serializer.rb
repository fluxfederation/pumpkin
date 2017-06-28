class BugSummarySerializer < ActiveModel::Serializer
  has_many :issues

  attributes :id, :message, :first_occurred_at, :last_occurred_at, :occurrence_count, :closed_at
end
