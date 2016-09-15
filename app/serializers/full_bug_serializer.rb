class FullBugSerializer < BugSerializer
  attributes :data

  def data
    object.primary_occurrence.data
  end
end
