class MakeOccurrencesMessageMandatory < ActiveRecord::Migration[5.0]
  def up
    execute "ALTER TABLE occurrences ALTER message SET NOT NULL"
  end

  def down
    execute "ALTER TABLE occurrences ALTER message DROP NOT NULL"
  end
end
