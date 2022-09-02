Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html

  match "/auth/twitter/callback",to: "sessions#create", via: [:get, :post]
  post "/signout", to: "sessions#destroy", as: :signout

  post "/entry", to: "entry#create"

  get "/entry/:challenge/delete/:entry",to: "entry#destroy", as: :delete_entry
  post "/entry/:challenge/comment/:entry", to: "entry#comment", as: :comment_entry

  resources :challenges do
    member do
      get 'user/:username', to: "challenges#user", as: :user, :constraints => { :username => /[^\/]+/ }
    end
  end

  get "/feed", to: "main#feed", defaults: {format: "rss"}
  get "/about", to: "main#about"

  # match "/top" => "users#top", as: :top
  # match "*unmatched_route", to: redirect('/about', status: 302), via: [:get, :post]
  get "/:username", to: "users#show", as: :profile, :constraints => { :username => /[^\/]+/ }

  root to: "main#index"

  # Sample resource route within a namespace:
  #   namespace :admin do
  #     # Directs /admin/products/* to Admin::ProductsController
  #     # (app/controllers/admin/products_controller.rb)
  #     resources :products
  #   end
end
