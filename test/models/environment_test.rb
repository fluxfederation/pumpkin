require 'test_helper'

class EnvironmentTest < ActiveSupport::TestCase
  test "it doesn't allow invalid env names" do
    env = Environment.new(id: "va-1-id-199")
    assert(env.valid?)

    # No blank names
    env.id = nil
    assert_not(env.valid?)

    env.id = ""
    assert_not(env.valid?)

    # No start hyphens
    env.id = "-invalid"
    assert_not(env.valid?)

    # No start periods
    env.id = ".invalid"
    assert_not(env.valid?)

    # No end hyphens
    env.id = "invalid-"
    assert_not(env.valid?)

    # No end periods
    env.id = "invalid."
    assert_not(env.valid?)

    #No odd chars
    env.id = "inva|id"
    assert_not(env.valid?)
  end
end
