Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
  resources :occurrences, only: [:create]
  resources :environments
  resources :bugs do
    member do
      post :close
      post :create_issue
    end

    resources :occurrences, only: [:index]
  end

  get '/monitoring/heartbeat', to: 'heartbeat#index'
end
