require 'test_helper'

class EnvironmentsControllerTest < ActionDispatch::IntegrationTest
  include ActiveJob::TestHelper
  include ActiveModelSerializers::Test::Schema

  setup do
    @default_environment_ids = [environments(:qa), environments(:prod)].map(&:to_param)
  end

  test "index returns most recently active first" do
    get environments_path
    assert_response :success
    results = JSON.parse(@response.body)
    assert_equal ['Production', 'QA'], results.map { |r| r['id'] }

    occurrences(:qa_normal).update!(occurred_at: Time.now)

    get environments_path
    assert_response :success
    results = JSON.parse(@response.body)
    assert_equal ['QA', 'Production'], results.map { |r| r['id'] }
  end
end
