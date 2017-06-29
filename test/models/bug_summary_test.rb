require 'test_helper'

class BugSummaryTest < ActiveSupport::TestCase
  test "#with_latest_details" do
    bug = bugs(:prod_twice)
    summary = BugSummary.find(bug.id)
    assert_equal events(:prod_twice_created).created_at, summary.latest_event_at
    assert_equal events(:prod_twice_created).name, summary.latest_event_name
    assert_equal bug.primary_occurrence.occurred_at, summary.first_occurred_at
    assert_equal occurrences(:prod_twice2).occurred_at, summary.last_occurred_at
    assert_equal occurrences(:prod_twice2).message, summary.message
    assert_equal bug.occurrences.count, summary.occurrence_count
    assert_equal nil, summary.closed_at

    new_event = bug.events.create!(name: "closed")
    summary = BugSummary.find(bug.id)
    assert_equal new_event.created_at, summary.latest_event_at
    assert_equal new_event.name, summary.latest_event_name
    assert_equal new_event.created_at, summary.closed_at

    new_occurrence = occurrences(:prod_twice2).dup
    new_occurrence.update_attributes!(occurred_at: Time.zone.now)
    summary = BugSummary.find(bug.id)
    assert_equal new_occurrence.occurred_at, summary.last_occurred_at
    assert_equal bug.occurrences.count, summary.occurrence_count

    # # doesnt look at occurrences for other bugs
    occurrence2 = occurrences(:prod_normal).dup
    occurrence2.update_attributes! occurred_at: Time.zone.now
    summary = BugSummary.find(bug.id)
    assert_equal new_occurrence.occurred_at, summary.last_occurred_at
  end
end
