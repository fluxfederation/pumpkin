class Patch < ApplicationRecord
  validates :name, presence: true

  has_many :occurrences
  has_many :bugs, through: :occurrences
end
