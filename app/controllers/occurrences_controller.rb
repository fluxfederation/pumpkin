class OccurrencesController < ApplicationController
  skip_before_action :redirect_if_not_google_authenticated, only: :create if Rails.env.production?
  before_action :authenticate!, only: :create

  def index
    bug = Bug.find(params[:bug_id])
    occurrences = bug.occurrences.order("occurred_at DESC")
    occurrences = occurrences.limit(params[:limit]) if params[:limit]
    render json: occurrences
  end

  def create
    environment = Environment.find_or_create_by!(id: params[:occurrence][:environment])

    occurrence = environment.occurrences.new(occurrence_params)
    occurrence.data = params[:occurrence].fetch(:data, {})
    occurrence.save!

    AssignBug.perform_later(occurrence.id)

    render json: occurrence, include: [:environment]
  end

  private

  def occurrence_params
    params.require(:occurrence).permit(:occurred_at, :message, :tags)
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
