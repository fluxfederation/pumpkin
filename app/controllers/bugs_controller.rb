class BugsController < ApplicationController
  def index
    bugs = if params[:patch_id]
      Patch.find(params[:patch_id]).bugs
    else
      Bug.all
    end

    render json: bugs, include: []
  end

  def show
    bug = Bug.find(params[:id])
    render json: bug
  end
end
