require 'rails/application_controller'

class StaticController < Rails::ApplicationController
  before_action :redirect_if_not_google_authenticated if Rails.env.production?
  
  def index
    render file: Rails.root.join('public', 'app.html')
  end
end