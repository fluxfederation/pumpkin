class CreateIssue < ApplicationJob
  queue_as = :default

  def perform(bug_id)
    Bug.find(bug_id).update_attributes! issue_url: "https://jira.powershophq.com/browse/FAKE-#{bug.id}"
  end
end
