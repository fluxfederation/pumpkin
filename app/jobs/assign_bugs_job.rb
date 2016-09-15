class AssignBugsJob < ApplicationJob
  queue_as :default

  def perform(occurrence)
    occurrence.with_lock do
      occurrence.bug = Bug.joins(:primary_occurrence).merge(Occurrence.where(patch_id: occurrence.patch, message: occurrence.message)).first
      if occurrence.bug.nil?
        occurrence.bug = Bug.create!(primary_occurrence: occurrence)
      end
      occurrence.save!
    end
  end
end
