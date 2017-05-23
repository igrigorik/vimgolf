Vimgolf::Application.routes.draw do

  match "/auth/twitter/callback",to: "sessions#create", via: [:get, :post]
  get "/signout", to: "sessions#destroy", as: :signout

  post "/entry", to: "entry#create"

  get "/entry/:challenge/delete/:entry",to: "entry#destroy", as: :delete_entry
  post "/entry/:challenge/comment/:entry", to: "entry#comment", as: :comment_entry

  resources :challenges

  get "/feed", to: "main#feed", defaults: {format: "rss"}
  get "/about", to: "main#about"

  # match "/top" => "users#top", as: :top
  # match "/:username" => "users#show", as: :profile
  match "*unmatched_route", to: redirect('/about', status: 302), via: [:get, :post]

  root to: "main#index"

  # Sample resource route within a namespace:
  #   namespace :admin do
  #     # Directs /admin/products/* to Admin::ProductsController
  #     # (app/controllers/admin/products_controller.rb)
  #     resources :products
  #   end
end
