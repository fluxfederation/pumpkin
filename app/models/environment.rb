class Environment < ApplicationRecord
  has_many :occurrences, dependent: :destroy
  has_many :bugs, through: :occurrences
end
