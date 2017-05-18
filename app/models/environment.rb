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
end
