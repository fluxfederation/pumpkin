class Environment < ApplicationRecord
  has_many :occurrences, dependent: :destroy
  has_many :bugs, through: :occurrences

  # Lifted (and extended) from Mr. Wippy, our WIP environment deployer
  validates :id,
    presence: true,
    length: { maximum: 30 }
  validates :id,
    format: { without: /\A[\-\.]/, message: "cannot start with a hyphen or dot" }
  validates :id,
    format: { without: /[\-\.]\z/, message: "cannot end with a hyphen or dot" }
  validates :id,
    format: { with: /\A[a-zA-Z0-9\.\-]+\z/, message: "must only contain letters, numbers, hyphens or periods" }

  scope :by_recent_activity, ->() { from(<<-end_sql) }
    (
      SELECT e.*,
        (
          SELECT occurrences.occurred_at AS last_occurred_at
          FROM occurrences WHERE occurrences.environment_id = e.id
          ORDER BY occurred_at DESC LIMIT 1
        )
      FROM environments AS e
      ORDER BY last_occurred_at DESC
    ) AS environments
  end_sql
end
