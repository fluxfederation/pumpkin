require_relative 'boot'

require "rails"
# Pick the frameworks you want:
require "active_model/railtie"
require "active_job/railtie"
require "active_record/railtie"
require "action_controller/railtie"
require "action_mailer/railtie"
require "action_view/railtie"
require "action_cable/engine"
# require "sprockets/railtie"
require "rails/test_unit/railtie"

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

module Pumpkin
  class Application < Rails::Application
    config.active_record.schema_format = :sql

    # Only loads a smaller set of middleware suitable for API only apps.
    # Middleware like session, flash, cookies can be added back manually.
    # Skip views, helpers and assets when generating a new resource.
    config.api_only = true

    config.middleware.use ActionDispatch::Cookies
    config.middleware.use ActionDispatch::Session::CookieStore

    config.active_job.queue_adapter = :resque

    SimpleGoogleAuth.configure do |config|
      config.client_id      = Rails.application.secrets.google_auth_client_id
      config.client_secret  = Rails.application.secrets.google_auth_client_secret
      config.redirect_uri   = Rails.application.secrets.google_auth_client_callback

      config.authenticate = lambda do |data|
        data.email.ends_with?("@#{Rails.application.secrets.google_auth_required_domain}")
      end
    end
  end
end
