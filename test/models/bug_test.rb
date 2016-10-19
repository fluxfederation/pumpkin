require 'test_helper'

class BugTest < ActiveSupport::TestCase
  test "#with_latest_event" do
    bug = Bug.with_latest_event.find(bugs(:prod_normal).id)
    assert_equal events(:prod_normal_created), bug.latest_event

    event = bug.events.create! name: "closed"
    bug = Bug.with_latest_event.find(bugs(:prod_normal).id)
    assert_equal event, bug.latest_event
  end
end
