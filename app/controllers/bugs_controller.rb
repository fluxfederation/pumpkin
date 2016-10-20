class BugsController < ApplicationController
  def index
    bugs = Bug.with_latest_details.includes({:occurrences => :patch}, {:primary_occurrence => :patch})
    bugs = bugs.where(:id => params[:ids]) if params[:ids]
    bugs = bugs.joins(:primary_occurrence).merge(Occurrence.where(:patch_id => params[:patch_ids])) if params[:patch_ids]
    render json: bugs, include: [:latest_event]
  end

  def show
    bug = Bug.with_latest_details.find(params[:id])
    render json: bug, serializer: FullBugSerializer
  end

  def close
    bug = Bug.with_latest_details.find(params[:id])
    bug.events.create!(name: 'closed')
    render json: bug, serializer: FullBugSerializer
  end
end
