class OccurrencesController < ApplicationController
  before_action :authenticate!, only: :create

  def index
    bug = Bug.find(params[:bug_id])

    render json: bug.occurrences
  end

  def create
    environment = Environment.find_or_create_by!(name: params[:occurrence][:pumpkin_environment])

    occurrence = environment.occurrences.new(occurrence_params)
    occurrence.data = params[:occurrence][:data]
    occurrence.save!

    AssignBugsJob.perform_later(occurrence)

    render json: occurrence, include: [:environment]
  end

  private

  def occurrence_params
    params.require(:occurrence).permit(:occurred_at, :message)
  end

  def authenticate!
    authorization = request.headers['Authorization']
    if authorization.blank?
      Rails.logger.warning "Unauthorized - missing token"
      render json: {message: "Unauthorized"}, status: :unauthorized
      return
    end
    raw_token = authorization.split(" ").last
    if !ActiveSupport::SecurityUtils.secure_compare(raw_token, Rails.application.secrets.auth_token)
      Rails.logger.warning "Unauthorized - token doesn't match"
      render json: {message: "Unauthorized"}, status: :unauthorized
      return
    end
  end
end
