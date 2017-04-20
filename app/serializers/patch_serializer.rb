class EnvironmentSerializer < ActiveModel::Serializer
  attributes :id, :name

  has_many :bugs
end
