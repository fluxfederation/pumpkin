class Occurrence < ApplicationRecord
  belongs_to :environment
  belongs_to :bug, optional: true
end
