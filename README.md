# [VimGolf.com](http://www.vimgolf.com) Client

Real Vim ninjas count every keystroke - do you? Head on over to vimgolf.com, pick a challenge, and show us what you've got! The rules are simple:

* Each challenge provides an input file, and an output file
* Your goal is to modify the input file such that it matches the output
* Once you install the vimgolf CLI, pick a challenge, open a prompt and put away!

When you launch a challenge from the command line, it will be downloaded from the site and a local Vim session will be launched, which will log every keystroke you make. Once you're done, simply *:wq* (write and quit) the session and we will score your input and upload it back to the site!

Let the games begin.

## Setup & Play

<pre>
1. $> gem install vimgolf
2. $> vimgolf setup (go to vimgolf.com, sign in, and grab your API key)
3. Pick a challenge on vimgolf.com
4. $> vimgolf put [challenge ID]
</pre>

## Todo's & Wishlist

* At the moment, scoring is done based on the simplest possible model: bytesize of your Vim script file. Instead, we'd like to assign a score based on shortcuts and key-combinations used. Ex: visual mode gets you extra x points, etc.
* Vim script parser - distinguish between different modes, keystrokes, etc.

Other patches, tips & ideas are welcome!

## License

(MIT License) - Copyright (c) 2010 Ilya Grigorik