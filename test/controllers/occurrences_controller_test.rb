require 'test_helper'

class OccurrencesControllerTest < ActionDispatch::IntegrationTest
  include ActiveJob::TestHelper
  include ActiveModelSerializers::Test::Schema

  test "index" do
    get bug_occurrences_path(bug_id: bugs(:prod_normal))
    assert_response :success
    assert_response_schema "occurrences/index.json"

    occurrences(:prod_normal)
  end

  test "create no token" do
    post occurrences_path
    assert_response :unauthorized
  end

  test "create bad token" do
    post occurrences_path, headers: {"Authorization" => "TOKEN very_bad_token"}
    assert_response :unauthorized
  end

  test "create" do
    assert_difference 'Occurrence.count', 1 do
      assert_enqueued_with(job: AssignBugsJob) do
        post occurrences_path, headers: {"Authorization" => "TOKEN #{Rails.application.secrets.auth_token}"}, params: {occurrence: {pumpkin_environment:"Normal", message:"Extremely normal", occurred_at:"2011-01-01"}}
      end
    end
    assert_response :success
    assert_response_schema "occurrences/create.json"
  end
end
