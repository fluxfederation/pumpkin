class HeartbeatController < ApplicationController
  skip_before_action :redirect_if_not_google_authenticated
  def index
    head :ok
  end
end
