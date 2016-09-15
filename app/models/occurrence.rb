class Occurrence < ApplicationRecord
  belongs_to :patch
  belongs_to :bug, optional: true
end
