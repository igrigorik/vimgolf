Vimgolf::Application.routes.draw do

  match "/auth/twitter/callback" => "sessions#create"
  match "/signout" => "sessions#destroy", :as => :signout

  match "/entry" => "entry#create"
  match "/entry/:challenge/delete/:entry" => "entry#destroy",
    :as => :delete_entry
  match "/entry/:challenge/vote/:entry/:direction" => "entry#vote",
    :as => :vote_on_entry
  match "/entry/:challenge/comment/:entry" => "entry#comment",
    :as => :comment_entry

  resources :challenges

  match "/feed" => "main#feed", :defaults => {:format => "rss"}
  match "/about" => "main#about"

  # match "/top" => "users#top", :as => :top
  # match "/:username" => "users#show", :as => :profile
  match "*unmatched_route", :to => redirect('/about', status: 302)

  root :to => "main#index"

  # Sample resource route within a namespace:
  #   namespace :admin do
  #     # Directs /admin/products/* to Admin::ProductsController
  #     # (app/controllers/admin/products_controller.rb)
  #     resources :products
  #   end
end
