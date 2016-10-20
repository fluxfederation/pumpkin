# config valid only for current version of Capistrano
lock '3.6.1'

set :application, 'com-test-pumpkin'

set :use_sudo, false
set :scm, :git
set :repo_url, 'git@git.powershop.co.nz:pumpkin/api.git'

set :bundle_without, [:development, :test, :deployment]

set(:deploy_to) { "/apps/com-test-pumpkin" }
before "bundler:install",       "deploy:fix_permissions"
# after  "git:create_release",    "deploy:link_global_app_env"

namespace :deploy do
  task :fix_permissions do
    on roles(:app) do
      execute :sudo, "/usr/local/bin/fix_deploy_permissions.sh com-test-pumpkin"
    end
  end

  task :require_tag do
    set :branch, ENV['tag'] || raise("Specify the tag to deploy using the tag variable: `cap tag=nn.nn #{application} deploy`")
    `git tag | grep -q #{ENV['tag']}`
    raise("Tag #{ENV['tag']} doesn't exist!") unless $?.success?
    refs = `git ls-remote --tags origin refs/tags/#{ENV['tag']}`
    raise("You forgot to push your tag. Run 'git push origin --tags'.") unless $?.success? && !refs.empty?
  end

  desc "make a TAG file"
  task :make_tag_file do
    run "umask 02 && echo '#{branch}' > #{release_path}/TAG"
  end

  desc "Link the /etc/app.env file to .env in the app directory so Dotenv uses it"
  task :link_global_app_env do
    run "ln -nfs /etc/app.env #{release_path}/.env"
  end
end