if Rails.env.production?
  Resque.redis = ENV.fetch('REDIS_ADDRESS')
end
