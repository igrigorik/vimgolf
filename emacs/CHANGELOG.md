## 0.10.0

2014-08-12 Siddhanathan Shanmugam <siddhanathan@gmail.com>

	* vimgolf.el (vimgolf-setup): Enable JSON parsing
	- Allow Emacs to parse JSON content from the site
	- Replace yaml parser with json parser
	- Use 'identitiy' accept encoding header to get uncompressed data from server

	* vimgolf.el (vimgolf-get-text): Add function
	- Extracts input and output text, used in vimgolf-setup

	* vimgolf.el (vimgolf-retrieve-challenge): Add function
	- Gets JSON data from server and reads it using a buffer

	* vimgolf.el (vimgolf-read-next-data-chunk): Removed function

## 0.9.3

2012-07-29 Brian Zwahr (github: @echosa)

	* vimgolf.el
	- Added challenge browser
	- Better description
	- Truncated titles
	- Basic handling of HTML entities
	- Fixed showing browse list after url-retrieve

	* features/*
	- Added documentation

	* util/*
	- Added tests via ecukes

## 0.9.2

2012-02-28 Steve Purcell (@sanityinc)

	* vimgolf.el
	- changes for adding to melpa

## 0.9.1

2012-02-27 Tim Visher (@timvisher)

	* vimgolf.el
	- Use vimgolf-continue to get messages when buffers are setup
	- Correct indentation and add local valiables
	- Updating notes and adding a contributors section

2012-01-08 Adam Collard (@acollard)

	* vimgolf.el
	- Simplify vimgolf-solution-correct-p to use buffer-string

## 0.9.0 and earlier

Lots of changes by Tim Visher and Steve Purcell
