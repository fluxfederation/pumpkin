class EnvironmentsController < ApplicationController
  def index
    render json: Environment.by_recent_activity, include: []
  end
end
