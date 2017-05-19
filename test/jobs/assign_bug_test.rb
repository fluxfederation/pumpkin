require 'test_helper'

class AssignBugTest < ActiveJob::TestCase
  test "existing bug assignment" do
    occurrence = occurrences(:prod_normal_unassigned)
    assert_no_difference 'Bug.count' do
      AssignBug.perform_now(occurrence.id)
    end
    assert_equal bugs(:prod_normal), occurrence.reload.bug
  end

  test "new bug assignment" do
    occurrence = occurrences(:prod_new_unassigned)
    assert_difference 'Bug.count', 1 do
      AssignBug.perform_now(occurrence.id)
    end
    bug = occurrence.reload.bug
    assert_equal 'created', bug.events.first!.name
    assert_equal occurrence, bug.primary_occurrence
  end

  test "bugs are environment independent" do
    prod_occurrence = occurrences(:prod_new_unassigned)
    qa_occurrence = occurrences(:qa_new_unassigned)

    assert_difference 'Bug.count', 1 do
      AssignBug.perform_now(prod_occurrence.id)
    end

    assert_no_difference 'Bug.count' do
      AssignBug.perform_now(qa_occurrence.id)
    end

    bug = prod_occurrence.reload.bug

    assert_equal bug, qa_occurrence.reload.bug
    assert_equal prod_occurrence, bug.primary_occurrence
  end
end
