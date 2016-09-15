require 'test_helper'

class BugsControllerTest < ActionDispatch::IntegrationTest
  include ActiveModelSerializers::Test::Schema

  test "index" do
    get bugs_path
    assert_response :success
    assert_response_schema "bugs/index.json"
  end

  test "show" do
    get bug_path(bugs(:normal))
    assert_response :success
    assert_response_schema "bugs/show.json"
  end
end
