class BugSerializer < ActiveModel::Serializer
  attributes :id, :message, :first_occurred_at, :last_occurred_at, :data

  has_many :occurrences

  class OccurrenceSerializer < ActiveModel::Serializer
    attributes :id
  end

  def message
    object.primary_occurrence.message
  end

  def first_occurred_at
    object.primary_occurrence.occurred_at
  end

  def last_occurred_at
    object.occurrences.order("occurred_at DESC").first.occurred_at
  end

  def data
    object.primary_occurrence.data
  end
end
