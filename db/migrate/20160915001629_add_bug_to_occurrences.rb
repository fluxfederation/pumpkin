class AddBugToOccurrences < ActiveRecord::Migration[5.0]
  def change
    add_reference :occurrences, :bug, type: :uuid
  end
end
