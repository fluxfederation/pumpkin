class UseNameAsEnvironmentPrimaryKey < ActiveRecord::Migration[5.0]
  def up
    execute <<-end_sql

ALTER TABLE environments ALTER name SET NOT NULL;

ALTER TABLE environments DROP CONSTRAINT patches_pkey;

ALTER TABLE environments RENAME COLUMN id TO uuid;

ALTER TABLE environments RENAME COLUMN name TO id;

ALTER TABLE environments ADD PRIMARY KEY (id);

ALTER TABLE occurrences ALTER COLUMN environment_id TYPE TEXT;

UPDATE occurrences
   SET environment_id = e.id
   FROM environments e
   WHERE environment_id = e.uuid :: TEXT;

ALTER TABLE occurrences
  ADD CONSTRAINT occurrences_environment_id_fk
   FOREIGN KEY (environment_id) REFERENCES environments(id);

ALTER TABLE environments DROP COLUMN uuid;

DROP INDEX index_environments_on_name;
    end_sql
  end

  def down
    raise IrreversibleMigration
  end
end
