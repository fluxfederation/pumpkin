require 'test_helper'

class BugsControllerTest < ActionDispatch::IntegrationTest
  include ActiveModelSerializers::Test::Schema

  test "index" do
    get bugs_path
    assert_response :success
    assert_response_schema "bugs/index.json"
  end

  test "index with bug filtering" do
    get bugs_path, params: {ids: [bugs(:prod_normal), bugs(:prod_twice)].map(&:to_param)}
    assert_response :success
    assert_response_schema "bugs/index.json"
  end

  test "index with patch filtering" do
    get bugs_path, params: {patch_ids: [patches(:qa)].map(&:to_param)}
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
end
