class Occurrence < ApplicationRecord
  belongs_to :environment
  belongs_to :bug, optional: true

  validates :occurred_at, presence: true

  validate :data_present

  private

  def data_present
    errors.add(:data, "can't be blank") unless data
  end
end
