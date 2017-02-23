class OccurrencesController < ApplicationController
  before_action :authenticate!, only: :create

  def index
    bug = Bug.find(params[:bug_id])

    render json: bug.occurrences
  end

  def create
    patch = Patch.find_or_create_by!(name: params[:occurrence][:pumpkin_patch])

    occurrence = patch.occurrences.new(occurrence_params)
    occurrence.data = params[:occurrence][:data]
    occurrence.save!

    AssignBugsJob.perform_later(occurrence)

    render json: occurrence, include: [:patch]
  end

  private

  def occurrence_params
    params.require(:occurrence).permit(:occurred_at, :message)
  end

  def authenticate!
    authorization = request.headers['Authorization']
    if authorization.blank?
      render json: {message: "Unauthorized"}, status: :unauthorized
      return
    end
    raw_token = authorization.split(" ").last
    if !ActiveSupport::SecurityUtils.secure_compare(raw_token, Rails.application.secrets.auth_token)
      render json: {message: "Unauthorized"}, status: :unauthorized
      return
    end
  end
end
