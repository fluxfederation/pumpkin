class OccurrencesController < ApplicationController

  def show
    occurrence = Occurrence.find(params[:id])
    render json: occurrence
  end

  def create
    patch = Patch.find_or_create_by!(name: params[:occurrence][:pumpkin_patch])

    occurrence = patch.occurrences.new(occurrence_params)
    occurrence.data = params[:occurrence][:data]
    occurrence.save!

    AssignBugsJob.perform_later(occurrence)

    render json: occurrence, include: []
  end

  private

  def occurrence_params
    params.require(:occurrence).permit(:occurred_at, :message)
  end
end
