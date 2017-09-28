class AddIndexOnOccurrencesForEnvironmentAndOccuredAt < ActiveRecord::Migration[5.0]
  def change
    add_index :occurrences, [:environment_id, :occurred_at], order: { environment_id: :asc, occured_at: :desc }
  end
end
