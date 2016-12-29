# [VimGolf.com](http://www.vimgolf.com) CLI Client

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
