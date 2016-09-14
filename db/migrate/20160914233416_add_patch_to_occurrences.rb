class AddPatchToOccurrences < ActiveRecord::Migration[5.0]
  def change
    add_reference :occurrences, :patch, type: :uuid
  end
end
