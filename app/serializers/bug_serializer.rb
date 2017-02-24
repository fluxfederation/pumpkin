class BugSerializer < ActiveModel::Serializer
  attributes :id, :message, :first_occurred_at, :last_occurred_at, :patch_id, :occurrence_count, :closed_at, :issue_url

  def message
    object.primary_occurrence.message
  end

  def first_occurred_at
    object.primary_occurrence.occurred_at
  end

  def patch_id
    object.primary_occurrence.patch.id
  end

  def occurrence_count
    object.occurrences.count
  end

  def closed_at
    return unless object.latest_event.try!(:name) == "closed"
    object.latest_event.created_at
  end
end
