class Occurrence < ApplicationRecord
  belongs_to :environment
  belongs_to :bug, optional: true

  validates :message, presence: true
  validates :occurred_at, presence: true

  validate :data_present

  scope :in_environments, ->(environment_ids) { where(environment_id: environment_ids) }
  scope :search, ->(query) { where("#{table_name}.message @@ ?", query) }

  private

  def data_present
    errors.add(:data, "can't be blank") unless data
  end
end
