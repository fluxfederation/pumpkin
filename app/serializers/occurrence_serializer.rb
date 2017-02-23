class OccurrenceSerializer < ActiveModel::Serializer
  attributes :id, :patch_id, :bug_id, :occurred_at, :message, :data
end
