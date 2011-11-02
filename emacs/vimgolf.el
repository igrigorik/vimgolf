;;; vimgolf.el --- VimGolf interface for the One True Editor

;; Copyright (C) never, by no one

;; Author: Tim Visher <tim.visher@gmail.com>
;; Maintainer: Tim Visher <tim.visher@gmail.com>
;; Created: 2011-11-02
;; Version: 0.1
;; Keywords: games vimgolf vim

;; This file is not part of GNU Emacs

;;; Commentary:

;; This is a simple package that allows Emacs users to compete on [VimGolf][1]
;; using the One True Editor. Competition can be commenced utilizing `M-x
;; vimgolf`. When finished with a challenge, `C-c C-v C-c` should finish your
;; editing, ensure correctness, and submit your score and keystrokes to
;; [VimGolf][1].
;;
;; On second thought, let's not go to Camelot. It's a silly place.

;;; Installation:

;; Use package.el. You'll need to add Marmalade to your archives:

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))

;; If you use a version of Emacs prior to 24 that doesn't include
;; package.el, you can get it from http://bit.ly/pkg-el23. If you have
;; an older package.el installed from tromey.com, you should upgrade
;; in order to support installation from multiple sources.

;;; License:

;; [CC BY-NC-SA 3.0](http://creativecommons.org/licenses/by-nc-sa/3.0/)

;;; Code:

;; Use dribble file to allow for running from within emacs
;; Main interface should be `M-x vimgolf Challenge ID: <Enter ID> RET <Do Your Editing> C-c C-v C-c`
;; Could also use C-c C-v o to get the challenge ID prompt

;; We don't load up a vanilla mode. This could be used for sharing useful general code snippets that

;; Important to emphasis a 'good sport' attitude where the community enforces an anti-special solution attitude

;; Load up two buffers. One with the solution, one with the start.
;; Open a dribble file
;; Make your edits
;; Press C-c C-v C-c
;; Close dribble file

;; Diff files

;; If different
;;   Echo fail!
;;   Pop open ediff
;; else
;;   Chomp off the last 3 keychords
;;   Parse dribble file
;;   Count keystrokes
;;   Echo You solved <Challenge> in <Strokes> keystrokes!
;;   Submit? (yes or no predicate):

;;   Echo w00t!
;;   

;;; vimgolf.el ends here
