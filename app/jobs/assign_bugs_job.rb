class AssignBugsJob < ApplicationJob
  queue_as :default

  def perform(occurrence)
    occurrence.with_lock do
      occurrence.bug = Bug.joins(:primary_occurrence).merge(Occurrence.where(patch_id: occurrence.patch, message: occurrence.message)).first
      if occurrence.bug.nil?
        occurrence.build_bug(primary_occurrence: occurrence)
        occurrence.bug.events.build(name: 'created')
        occurrence.bug.save!
      end
      occurrence.save!
    end
  end
end
