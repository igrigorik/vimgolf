# [VimGolf.com](http://www.vimgolf.com)

[![Build Status](https://travis-ci.org/igrigorik/vimgolf.svg?branch=master)](https://travis-ci.org/igrigorik/vimgolf) [![Code Climate](https://codeclimate.com/github/igrigorik/vimgolf/badges/gpa.svg)](https://codeclimate.com/github/igrigorik/vimgolf) [![Issue Count](https://codeclimate.com/github/igrigorik/vimgolf/badges/issue_count.svg)](https://codeclimate.com/github/igrigorik/vimgolf)

Real Vim ninjas count every keystroke - do you? Head on over to vimgolf.com, pick a challenge, and show us what you've got!

* Each challenge provides an input file, and an output file
* Your goal is to modify the input file such that it matches the output
* Once you install the vimgolf CLI, pick a challenge, open a prompt and put away!

When you launch a challenge from the command line, it will be downloaded from the site and a local Vim session will be launched, which will log every keystroke you make. Once you're done, simply *:wq* (write and quit) the session and we will score your input and upload it back to the site!

## Setup & Play

```bash
$> gem install vimgolf

(Go to vimgolf.com, sign in, and grab your API key)
$> vimgolf setup

(Pick a challenge on vimgolf.com)
$> vimgolf put [challenge ID]
```

# Playing from other editors

## Emacs

There's a lightly maintained interface to play VimGolf challenges in Emacs
over at [vimgolf.el](https://github.com/timvisher/vimgolf.el)

# VimGolf.com web app



```bash
# start local server
$> bundle exec unicorn -c config/unicorn.rb -E development

# deploy to Heroku
$> git push heroku web:master
```

## Run tests

Go to the root folder and run `bundle exec rake`.

# How to contribute

This part is for people not familliar with ruby ecosystem

## Installation

You need :
- ruby
- gem bundler
- mongodb
- Seed database

### ruby

You need a ruby version, same as .ruby-version

I recommend using rbenv, see https://github.com/rbenv/rbenv to install and manage ruby version.

```
cd $VIMGOLF_PATH
rbenv install
```

### bundler

Bundler is a gem that aim to manage gem per project.

```
gem install bundler
```

Then you need all gems for the apps

```
bundle install
```

### mongo-db

It is possible to get a fully functionnal mongodb instance through docker.

```
docker pull mongo

# Database will stay here. So we can stop/delete docker instance
sudo mkdir -p /opt/mongodb/db

# Start container
docker run -p 27017:27017 -v /opt/mongodb/db:/data/db --name my-mongo-dev -d mongo mongod --auth

# connect to database
docker exec -it my-mongo-dev mongo

# create admin user
use admin
db.createUser({
  user: "userAdmin",
  pwd: "password",
  roles: [{role: "userAdminAnyDatabase", db: "admin"}]
})
# exit

# connect to database
docker exec -it my-mongo-dev mongo 127.0.0.1/vimgolf_development -u userAdmin -p password --authenticationDatabase admin

# create vimgolf_development user
use vimgolf_development
db.createUser({
  user: "vimgolf_development",
  pwd: "vimgolf_development",
  roles: ["dbOwner"]
})

# create vimgolf_test user
use vimgolf_test
db.createUser({
  user: "vimgolf_test",
  pwd: "vimgolf_test",
  roles: ["dbOwner"]
})
# exit

# test connection
docker exec -it my-mongo-dev mongo 127.0.0.1/vimgolf_development -u vimgolf_development -p vimgolf_development --authenticationDatabase vimgolf_development
# we are connected, we can exit
```

### Seed database

Read db/seeds.rb to understand params

```
# make the index page works - create collection "challenges"
bundle exec rake db:drop db:setup

# or add many challenges users and entries
bundle exec rake db:drop db:setup challenges=40 users=30 entries=20
```

## Run the app

Start mongo-db this way if you are using the docker installation :

```
docker run -p 27017:27017 -v /opt/mongodb/db:/data/db --name my-mongo-dev -d mongo mongod --auth
# if you see:
# docker: Error response from daemon: Conflict. The container name "/my-mongo-dev" is already in use by container "a9
# then you have to remove the container
docker rm my-mongo-dev
```

Start the server

```
bundle exec unicorn -c config/unicorn.rb -E development
```

open your browser to http://localhost:8080/

If you need more logs, you can

```
docker logs -f my-mongo-dev
tail -f log/development.log
```

To see mongodb query :
```
# log to database
docker exec -it mongo-3.6 mongo 127.0.0.1/vimgolf_development -u vimgolf_development -p vimgolf_development --authenticationDatabase vimgolf_development

# Change profile level
db.setProfilingLevel(2)

# Call this after any query to get the last query
db.system.profile.find().pretty()
```

## Troubleshooting

### Cannot sign in with twitter

When click on 'sign in with twitter' and you get

```
OAuth::Unauthorized
401 Authorization Required
```

As a workaround, you can edit

```
# in app/controllers/application_controller.rb
# replace
    @current_user ||= User.where(uid: session[:user]).first if session[:user]
# with
    @current_user ||= User.first
```

### Cannot run tests

If you installed mongodb through this tutorial, you created a database 'vimgolf_test' with user and password
set to 'vimgolf_test'.
You need to update config/mongoid.yml with the credentials :

```yaml
test:
  clients:
    default:
      options:
        user: 'vimgolf_test'
        password: 'vimgolf_test'
```

## Find documentation

It use gem mongoid which is a wrapper for the mongo driver

Mongodb ruby driver : http://api.mongodb.com/ruby/current/Mongo.html

Mongoid: https://docs.mongodb.com/mongoid/master
