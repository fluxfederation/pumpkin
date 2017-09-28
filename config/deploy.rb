# config valid only for current version of Capistrano
lock '3.6.1'

set :application, 'pumpkin'

set :pty, true
set :shell, "/bin/bash"
set :use_sudo, false
set :scm, :git
set :repo_url, 'git@github.com:fluxfederation/pumpkin.git'
set :branch, ENV['BRANCH'] if ENV['BRANCH']
set :bundle_without, [:development, :test, :deployment]
set :passenger_restart_with_touch, true

set(:deploy_to) { "/apps/#{fetch(:application)}" }

set(:linked_dirs) { %w(log tmp/cache) }

set :rails_env, 'production'

before "bundler:install", "deploy:fix_permissions"
before "deploy:assets:precompile", "deploy:link_global_app_env"
before "deploy:migrate", "workers:stop"
before "passenger:restart", "workers:start"

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

namespace :workers do
  desc "Stop Resque workers"
  task :stop do
    on roles(:app) do
      execute :sudo, "service resque stop"
    end
  end

  desc "Start Resque workers"
  task :start do
    on roles(:app) do
      execute :sudo, "service resque start"
    end
  end
end
