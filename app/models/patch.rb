class Patch < ApplicationRecord
  validates :name, presence: true

  has_many :occurrences
end
