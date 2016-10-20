require 'test_helper'

class BugTest < ActiveSupport::TestCase
  test "#with_latest_details" do
    bug = Bug.with_latest_details.find(bugs(:prod_twice).id)
    assert_equal events(:prod_twice_created), bug.latest_event
    assert_equal occurrences(:prod_twice2).occurred_at, bug.last_occurred_at

    event = bug.events.create! name: "closed"
    bug = Bug.with_latest_details.find(bugs(:prod_twice).id)
    assert_equal event, bug.latest_event
    assert_equal occurrences(:prod_twice2).occurred_at, bug.last_occurred_at

    occurrence = occurrences(:prod_twice2).dup
    occurrence.update_attributes! occurred_at: Time.zone.now
    bug = Bug.with_latest_details.find(bugs(:prod_twice).id)
    assert_equal event, bug.latest_event
    assert_equal occurrence.occurred_at, bug.last_occurred_at
  end
end
