require 'test_helper'

class BugsControllerTest < ActionDispatch::IntegrationTest
  include ActiveJob::TestHelper
  include ActiveModelSerializers::Test::Schema

  setup do
    @default_patch_ids = [patches(:qa), patches(:prod)].map(&:to_param)
  end

  test "index with no patch ids" do
    get bugs_path
    assert_response :success

    assert_empty JSON.parse(@response.body)
  end

  test "index with patch ids" do
    get bugs_path, params: {patch_ids: @default_patch_ids}
    assert_response :success
    assert_response_schema "bugs/index.json"

    event_times = JSON::parse(@response.body).map { |bug| Time.parse(bug['last_occurred_at']) }
    assert_equal event_times, event_times.sort.reverse
  end

  test "index with bug filtering" do
    get bugs_path, params: {patch_ids: @default_patch_ids, ids: [bugs(:prod_normal), bugs(:prod_twice)].map(&:to_param)}
    assert_response :success
    assert_response_schema "bugs/index.json"
  end

  test "index with patch filtering" do
    get bugs_path, params: {patch_ids: [patches(:qa)].map(&:to_param)}
    assert_response :success
    assert_response_schema "bugs/index.json"
  end

  test "index with closed filtering" do
    get bugs_path, params: {patch_ids: @default_patch_ids, closed: "true"}
    assert_response :success
    assert_response_schema "bugs/index.json"
  end

  test "index with not closed filtering" do
    get bugs_path, params: {patch_ids: @default_patch_ids, closed: "false"}
    assert_response :success
    assert_response_schema "bugs/index.json"
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
    assert_enqueued_with(job: CreateBugIssue) do
      post create_issue_bug_path(bugs(:prod_normal))
    end
    assert_response :success
    assert_response_schema "bugs/show.json"
  end
end
