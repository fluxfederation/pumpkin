class CreateIssue < ActiveJob::Base
  queue_as = :default

  def perform(bug)
    bug.update_attributes! issue_url: "https://jira.powershophq.com/browse/FAKE-#{bug.id}"
  end
end
