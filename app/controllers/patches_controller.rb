class PatchesController < ApplicationController
  def index
    render json: Patch.all
  end

  def show
    render json: Patch.find(params[:id])
  end
end
