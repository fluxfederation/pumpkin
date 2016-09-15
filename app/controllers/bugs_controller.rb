class BugsController < ApplicationController
  def index
    bugs = Bug.all
    bugs = bugs.where(:id => params[:ids]) if params[:ids]
    bugs = bugs.join(:primary_occurrence).merge(Occurrence.where(:patch_id => params[:patch_ids])) if params[:patch_ids]
    render json: bugs, include: []
  end

  def show
    bug = Bug.find(params[:id])
    render json: bug
  end
end
