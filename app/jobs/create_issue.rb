class CreateIssue
  @queue = :default

  def self.perform(bug)
    bug.update_attributes! issue_url: "https://jira.powershophq.com/browse/FAKE-#{bug.id}"
  end
end
