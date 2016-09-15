class OccurrenceSerializer < ActiveModel::Serializer
  attributes :id, :message, :data

  has_one :patch
  has_one :bug
end
