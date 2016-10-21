Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
  resources :occurrences
  resources :patches
  resources :bugs do
    member do
      post :close
      post :create_issue
    end
  end
end
