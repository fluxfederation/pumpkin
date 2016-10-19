class BugSerializer < ActiveModel::Serializer
  attributes :id, :message, :first_occurred_at, :last_occurred_at, :patch_id, :occurrence_count

  has_many :occurrences
  belongs_to :latest_event

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

  def patch_id
    object.primary_occurrence.patch.id
  end

  def occurrence_count
    object.occurrences.count
  end
end
