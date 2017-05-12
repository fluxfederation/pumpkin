class EnvironmentSerializer < ActiveModel::Serializer
  attributes :id

  has_many :bugs
end
