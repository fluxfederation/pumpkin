require 'test_helper'

class OccurrencesControllerTest < ActionDispatch::IntegrationTest
  include ActiveJob::TestHelper
  include ActiveModelSerializers::Test::Schema

  test "show" do
    get occurrence_path(occurrences(:normal))
    assert_response :success
    assert_response_schema "occurrences/show.json"
  end

  test "create" do
    assert_difference 'Occurrence.count', 1 do
      assert_enqueued_with(job: AssignBugsJob) do
        post occurrences_path, params: {occurrence: {pumpkin_patch:"Normal", message:"Extremely normal", occurred_at:"2011-01-01"}}
      end
    end
    assert_response :success
    assert_response_schema "occurrences/create.json"
  end
end
