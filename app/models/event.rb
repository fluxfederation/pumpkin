class Event < ApplicationRecord
  NAMES = ['created', 'closed']

  belongs_to :bug

  validates :name, inclusion: { in: NAMES }
end
