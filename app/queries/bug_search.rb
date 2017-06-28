class BugSearch
  attr_reader :bugs

  def initialize(ids: nil, environment_ids: nil, closed: false, limit: nil, start: nil, search: nil)
    @bugs = BugSummary
               .in_occurrence_order
               .includes(:issues)

    occurrence_match = Occurrence.where("bug_id = bug_summaries.id").in_environments(Array(environment_ids))
    occurrence_match = occurrence_match.search(search) if search.present?

    @bugs = @bugs.where(occurrence_match.select(1).exists)

    @bugs = bugs.where(id: ids) if ids

    @bugs = @bugs.limit(limit) if limit

    @bugs = @bugs.not_newer_than_bug(start) if start

    @bugs = bugs.open unless closed
  end
end
