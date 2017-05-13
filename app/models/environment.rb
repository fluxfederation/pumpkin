class Environment < ApplicationRecord
  validates :name, presence: true

  has_many :occurrences, dependent: :destroy
  has_many :bugs, through: :occurrences
end
