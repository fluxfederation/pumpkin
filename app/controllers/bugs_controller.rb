class BugsController < ApplicationController
  def index
    bugs = Bug.with_latest_details.order('last_occurred_at DESC').includes(:primary_occurrence => :patch)
    bugs = bugs.where(:id => params[:ids]) if params[:ids]
    bugs = bugs.joins(:primary_occurrence).merge(Occurrence.where(:patch_id => params[:patch_ids])) if params[:patch_ids]
    if params[:closed] == "true"
      bugs = bugs.where("latest_event_name = ?", "closed")
    else
      bugs = bugs.where("latest_event_name <> ?", "closed")
    end
    render json: bugs, include: []
  end

  def show
    bug = fetch_bug
    render json: bug, serializer: FullBugSerializer
  end

  def close
    bug = fetch_bug
    bug.events.create!(name: 'closed')
    render json: fetch_bug, serializer: FullBugSerializer
  end

  private
  def fetch_bug
    Bug.with_latest_details.find(params[:id])
  end
end
