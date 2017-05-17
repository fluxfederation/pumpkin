class ApplicationController < ActionController::API
  before_action :redirect_if_not_google_authenticated if Rails.env.production?
end
