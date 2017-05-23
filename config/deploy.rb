# config valid only for current version of Capistrano
lock '3.6.1'

set :application, 'pumpkin'

set :pty, true
set :shell, "/bin/bash"
set :use_sudo, false
set :scm, :git
set :repo_url, 'git@git.powershop.co.nz:pumpkin/api.git'
set :branch, ENV['BRANCH'] if ENV['BRANCH']
set :bundle_without, [:development, :test, :deployment]
set :passenger_restart_with_touch, true

set(:deploy_to) { "/apps/#{fetch(:application)}" }

set(:linked_dirs) { %w(log tmp/cache) }

set :rails_env, 'production'

before "bundler:install", "deploy:fix_permissions"
before "deploy:assets:precompile",  "deploy:link_global_app_env"

namespace :deploy do
  task :fix_permissions do
    on roles(:app) do
      execute :sudo, "/usr/local/bin/fix_deploy_permissions.sh #{fetch(:application)}"
    end
  end

  desc "Link the /etc/app.env file to .env in the app directory so Dotenv uses it"
  task :link_global_app_env do
    on roles(:app) do
      execute "ln -nfs /etc/app.env #{release_path}/.env"
    end
  end
end
