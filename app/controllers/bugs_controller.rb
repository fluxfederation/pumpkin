class BugsController < ApplicationController
  def index
    search = BugSearch.new(
      ids: params[:ids],
      environment_ids: params[:environment_ids],
      closed: (params[:closed] == "true"),
      search: params[:search].presence,
      limit: params[:limit],
      start: params[:start]
    )
    render json: search.bugs, include: [:issues]
  end

  def show
    render_summary
  end

  def close
    bug.events.create!(name: 'closed')
    render_summary
  end

  def create_issue
    bug.issues.create!(url: params[:url])
    render_summary
  end

  def delete_issue
    bug.issues.find(params[:issue_id]).destroy
    render_summary
  end

  private

  def bug
    Bug.find(params[:id])
  end

  def render_summary
    render json: BugSummary.includes(:issues).find(params[:id]), include: [:issues]
  end
end
