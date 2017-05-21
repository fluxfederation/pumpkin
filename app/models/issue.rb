class Issue < ApplicationRecord
  belongs_to :bug
  validates :url, presence: true
end
