class AddIssueUrlToBugs < ActiveRecord::Migration[5.0]
  def change
    add_column :bugs, :issue_url, :string
  end
end
