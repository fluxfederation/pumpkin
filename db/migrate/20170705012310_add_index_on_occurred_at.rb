class AddIndexOnOccurredAt < ActiveRecord::Migration[5.0]
  def change
    add_index :occurrences, :occurred_at
  end
end
