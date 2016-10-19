class Event < ApplicationRecord
  NAMES = ['created', 'closed']

  belongs_to :bug

  validates :name, inclusion: { in: NAMES }

  def self.created_order
    order('events.created_at ASC')
  end
end
