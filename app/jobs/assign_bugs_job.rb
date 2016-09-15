class AssignBugsJob < ApplicationJob
  queue_as :default

  def perform(*args)
    occurrence = args[0]
    occurrence.with_lock do
      occurrence.bug = Bug.joins(:primary_occurrence).merge(Occurrence.where(patch_id: occurrence.patch, message: occurrence.message)).first
      if occurrence.bug.nil?
        occurrence.bug = Bug.create!(primary_occurrence: occurrence)
      end
      occurrence.save!
    end
  end
end
