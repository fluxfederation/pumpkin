class BugSearch
  attr_reader :bugs

  def initialize(ids: nil, environment_ids: nil, closed: false, limit: nil)
    @bugs = Bug.with_latest_details.in_occurrence_order.with_occurrence_includes

    @bugs = bugs.where(:id => ids) if ids

    @bugs = bugs.with_primary_occurrence_in_environment(environment_ids)

    @bugs = @bugs.limit(limit) if limit

    @bugs = closed ? bugs.closed : bugs.open
  end
end
