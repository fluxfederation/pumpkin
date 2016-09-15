class BugsController < ApplicationController
  def index
    bugs = if params[:patch_id]
      Patch.find(params[:patch_id]).bugs
    else
      Bug.all
    end

    render json: bugs
  end
end
