class Environment < ApplicationRecord
  validates :name, presence: true

  has_many :occurrences
  has_many :bugs, through: :occurrences
end
