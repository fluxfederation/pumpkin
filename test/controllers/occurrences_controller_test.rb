require 'test_helper'

class OccurrencesControllerTest < ActionDispatch::IntegrationTest
  include ActiveJob::TestHelper
  include ActiveModelSerializers::Test::Schema

  test "index" do
    get bug_occurrences_path(bug_id: bugs(:prod_normal))
    assert_response :success
    assert_response_schema "occurrences/index.json"

    parsed = JSON::parse(@response.body)
    assert_equal parsed, parsed.sort_by { |o| o['occurred_at'] }.reverse
  end

  test "index with limit" do
    get bug_occurrences_path(bug_id: bugs(:prod_normal), limit: 1)
    assert_response :success
    assert_response_schema "occurrences/index.json"
    assert_equal 1, JSON::parse(@response.body).size
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
      assert_enqueued_with(job: AssignBug) do
        post occurrences_path, headers: {"Authorization" => "TOKEN #{Rails.application.secrets.auth_token}"}, params: {occurrence: {environment:"Normal", message:"Extremely normal", tags: %w(foo bar), occurred_at:"2011-01-01"}}
      end
    end
    assert_response :success
    assert_response_schema "occurrences/create.json"
  end
end
