Vimgolf::Application.routes.draw do

  match "/auth/twitter/callback" => "sessions#create", via: [:get, :post]
  match "/signout" => "sessions#destroy", :as => :signout, via: [:get, :post]

  match "/entry" => "entry#create", via: [:get, :post]
  match "/entry/:challenge/delete/:entry" => "entry#destroy",
    :as => :delete_entry, via: [:get, :post]
  match "/entry/:challenge/comment/:entry" => "entry#comment",
    :as => :comment_entry, via: [:get, :post]

  resources :challenges

  match "/feed" => "main#feed", :defaults => {:format => "rss"}, via: [:get, :post]
  match "/about" => "main#about", via: [:get, :post]

  # match "/top" => "users#top", :as => :top
  # match "/:username" => "users#show", :as => :profile
  match "*unmatched_route", :to => redirect('/about', status: 302), via: [:get, :post]

  root :to => "main#index"

  # Sample resource route within a namespace:
  #   namespace :admin do
  #     # Directs /admin/products/* to Admin::ProductsController
  #     # (app/controllers/admin/products_controller.rb)
  #     resources :products
  #   end
end
