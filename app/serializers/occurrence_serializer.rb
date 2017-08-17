class OccurrenceSerializer < ActiveModel::Serializer
  attributes :id, :environment_id, :bug_id, :occurred_at, :message, :tags, :data
end
