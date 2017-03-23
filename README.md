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

Go to the root folder and run `rake`.
