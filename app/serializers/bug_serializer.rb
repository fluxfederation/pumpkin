class BugSerializer < ActiveModel::Serializer
  attributes :id, :message, :occurred_at, :data

  has_many :occurrences

  class OccurrenceSerializer < ActiveModel::Serializer
    attributes :id
  end

  def message
    object.primary_occurrence.message
  end

  def occurred_at
    object.primary_occurrence.occurred_at
  end

  def data
    object.primary_occurrence.data
  end
end
