require 'test_helper'

class BugsControllerTest < ActionDispatch::IntegrationTest
  include ActiveJob::TestHelper
  include ActiveModelSerializers::Test::Schema

  setup do
    @default_environment_ids = [environments(:qa), environments(:prod)].map(&:to_param)
  end

  test "index with no environment ids" do
    get bugs_path
    assert_response :success

    assert_empty JSON.parse(@response.body)
  end

  test "index with environment ids" do
    get bugs_path, params: {environment_ids: @default_environment_ids}
    assert_response :success
    assert_response_schema "bugs/index.json"

    event_times = JSON::parse(@response.body).map { |bug| Time.parse(bug['last_occurred_at']) }
    assert_equal event_times, event_times.sort.reverse
  end

  test "index with bug filtering" do
    get bugs_path, params: {environment_ids: @default_environment_ids, ids: [bugs(:prod_normal), bugs(:prod_twice)].map(&:to_param)}
    assert_response :success
    assert_response_schema "bugs/index.json"
  end

  test "index with limit" do
    get bugs_path, params: {environment_ids: @default_environment_ids, limit: "2"}
    assert_response :success
    assert_response_schema "bugs/index.json"
    assert_equal 2, JSON::parse(@response.body).size
  end

  test "index with limit and start" do
    get bugs_path, params: {environment_ids: @default_environment_ids, limit: "2", closed: "true", start: bugs(:prod_closed).id}
    assert_response :success
    assert_response_schema "bugs/index.json"
    assert_equal 1, JSON::parse(@response.body).size
  end

  test "index with environment filtering" do
    get bugs_path, params: {environment_ids: [environments(:qa)].map(&:to_param)}
    assert_response :success
    assert_response_schema "bugs/index.json"
  end

  test "index with closed filtering" do
    get bugs_path, params: {environment_ids: @default_environment_ids, closed: "true"}
    assert_response :success
    assert_response_schema "bugs/index.json"

    json = JSON.parse(@response.body)
    ids = json.collect {|bug| bug["id"]}

    # We include both open and closed when closed is enabled
    assert_includes(ids, bugs(:prod_normal).id)
    assert_includes(ids, bugs(:prod_closed).id)
  end

  test "index with not closed filtering" do
    get bugs_path, params: {environment_ids: @default_environment_ids, closed: "false"}
    assert_response :success
    assert_response_schema "bugs/index.json"

    json = JSON.parse(@response.body)
    ids = json.collect {|bug| bug["id"]}

    assert_includes(ids, bugs(:prod_normal).id)
    assert_not_includes(ids, bugs(:prod_closed).id)
  end

  test "index with matching search clauses" do
    get bugs_path, params: {environment_ids: @default_environment_ids, search: "Pumpkin"}
    assert_response :success
    assert_response_schema "bugs/index.json"

    messages = JSON::parse(@response.body).map { |bug| bug["message"] }
    # Even though there's two occurrences we should only be finding one bug
    assert_equal ["Pumpkin"], messages
  end

  test "index with no matching search clauses" do
    get bugs_path, params: {environment_ids: @default_environment_ids, search: "Squash"}
    assert_response :success
    assert_empty JSON.parse(@response.body)
  end

  test "index with matching search clauses filters by environment" do
    get bugs_path, params: {environment_ids: environments(:qa).to_param, search: "normal"}
    assert_response :success
    assert_response_schema "bugs/index.json"

    messages = JSON::parse(@response.body).map { |bug| bug["message"] }
    assert_equal [occurrences(:qa_normal).message], messages
  end

  test "show" do
    get bug_path(bugs(:prod_normal))
    assert_response :success
    assert_response_schema "bugs/show.json"
  end

  test "close" do
    post close_bug_path(bugs(:prod_normal))
    assert_response :success
    assert_response_schema "bugs/show.json"
    assert_equal "closed", bugs(:prod_normal).events.created_order.last!.name
  end

  test "create_issue" do
    issues_count = bugs(:prod_normal).issues.count
    issue_url =  "https://tracker.com/browse/CI-111111"
    post create_issue_bug_path(bugs(:prod_normal), url: issue_url)

    assert_response :success
    assert_response_schema "bugs/show.json"
    assert_equal (issues_count + 1), bugs(:prod_normal).issues.count
    assert_includes(bugs(:prod_normal).issues.pluck(:url), issue_url)
  end

  test "delete_issue" do
    issues_count = bugs(:prod_normal).issues.count
    post delete_issue_bug_path(bugs(:prod_normal), issue_id: issues(:prod_normal))
    assert_response :success
    assert_response_schema "bugs/show.json"
    assert_equal (issues_count - 1), bugs(:prod_normal).issues.count
  end
end
