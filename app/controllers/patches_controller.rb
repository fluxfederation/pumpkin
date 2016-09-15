class PatchesController < ApplicationController
  def index
    render json: Patch.all, include: []
  end
end
