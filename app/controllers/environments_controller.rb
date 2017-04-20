class EnvironmentsController < ApplicationController
  def index
    render json: Environment.all, include: []
  end
end
