class OccurrencesController < ApplicationController

  def show
    occurrence = Occurrence.find(params[:id])
    render json: occurrence
  end

  def create
    occurrence = Occurrence.new(occurrence_params)
    occurrence.data = params[:occurrence][:data]
    occurrence.save!
    render json: occurrence
  end

  private

  def occurrence_params
    params.require(:occurrence).permit(:occurred_at, :message)
  end
end
