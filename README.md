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

## Golfing without ruby installation: [use docker](https://hub.docker.com/r/hettomei/vimgolf/)

```
$> docker run --rm -it -e "key=YOUR_VIMGOLF_KEY" hettomei/vimgolf challenge_ID
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
$> git push heroku master
```

## Run tests

Go to the root folder and run `bundle exec rake`.

# How to contribute

This part is for people not familliar with ruby ecosystem

## Installation

You need:
- ruby
- gem bundler
- sqlite3 (library version 3.25 or newer, on your local system); or
- postgresql

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
gem install bundler:1.17.2
```

Then you need all gems for the apps

```
bundle install
```

## Using sqlite3 for development (and testing)

It is possible to get a fully functionnal app running locally using SQLite3 only. This is the default adapter for development and testing (it makes for easier ramp up, since you don't need to set up an instance of Postgres and create the appropriate users.)

Note that due to the use of SQL window functions, you need an SQLite3 library version 3.25 or newer installed on your system. The Ruby gem will use the locally installed library, so that is a pre-requisite to using SQLite3. The SQLite3 library shipped on Ubuntu 18.04 is too old, the one on Ubuntu 20.04 is new enough.

## Using Postgres for development

You can also use a Postgres database for development (and testing.)

Install and start a Postgres instance running locally on your development host.

The app will run under your user, so create a Postgres user with the same name as your local user and give that user the ability to create databases. This is usually accomplished with the following command:

```
sudo -u postgres createuser -d $(whoami)
```

When running any of the Ruby and Rails code, in order to use the Postgres adapter in the development and test environments, export environment variable `DATABASE_ADAPTER=pg`.

The database names configured will be `vimgolf_development` and `vimgolf_test` for the respective environments.

## Seed database

Read db/seeds.rb to understand parameters to define how many users, challenges and entries to create.

When creating a local development database on SQLite3 (the default adapter), use the following procedure:

```
# make the index page works - create collection "challenges"
bundle exec rails db:drop db:setup

# or add many challenges users and entries
bundle exec rails db:drop db:setup challenges=40 users=30 entries=20
```

When creating a local development database on Postgres, **do not use** the `db:setup` and `db:reset` targets, as those targets rely on loading the `db/schema.rb` file, but that file contains a schema dump compatible with the SQLite3 database and would possibly produce a broken setup under Postgres. Instead, use `db:migrate` and `db:migrate:reset` to create the database running through the Rails migrations as appropriate.

```
# under postgres
DATABASE_ADAPTER=pg bundle exec rails db:migrate:reset
DATABASE_ADAPTER=pg bundle exec rails db:seed challenges=40 users=30 entries=20
```

## Run the app

Start the server:

```
bundle exec unicorn -c config/unicorn.rb -E development
```

(If using Postgres, make sure you're passing it `DATABASE_ADAPTER=pg` through the environment.)

Open your browser to [localhost:8080](http://localhost:8080/).

You can check the Rails logs under:

```
tail -f log/development.log
```

## Logged in user under Development

When click on 'Sign In with Twitter' under development, you will almost certainly get:

```
OAuth::Unauthorized
401 Authorization Required
```

As a workaround, you can edit file `app/controllers/application_controller.rb` and replace:

```ruby
    @current_user ||= User.where(uid: session[:user]).first if session[:user]
```

With:

```ruby
    @current_user ||= User.first
```

Or, if you have a particular user in mind:

```ruby
    @current_user ||= User.find_by_nickname('myuser')
```

