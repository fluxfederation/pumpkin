class Bug < ApplicationRecord
  belongs_to :primary_occurrence, class_name: 'Occurrence'
  has_many :events
  has_many :occurrences
  has_many :issues
end
