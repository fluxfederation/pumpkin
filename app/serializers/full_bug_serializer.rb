class FullBugSerializer < BugSerializer
  has_many :issues

  attributes :data

  def data
    object.primary_occurrence.data
  end
end
