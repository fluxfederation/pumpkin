class ChangeDefaultValueForOccurrencesDataToEmptyHash < ActiveRecord::Migration[5.0]
  def change
    change_column_default(:occurrences, :data, {})
  end
end
