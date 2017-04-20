class BugSearch
  attr_reader :bugs

  def initialize(ids: nil, environment_ids: nil, closed: false)
    @bugs = Bug.with_latest_details.in_occurrance_order.with_occurrance_includes

    @bugs = bugs.where(:id => ids) if ids

    @bugs = bugs.with_primary_occurrence_in_environment(environment_ids)

    @bugs = closed ? bugs.closed : bugs.open
  end
end
