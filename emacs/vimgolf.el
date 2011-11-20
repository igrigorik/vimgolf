;;; vimgolf.el --- VimGolf interface for the One True Editor

;; Copyright (C) never, by no one

;; Author: Tim Visher <tim.visher@gmail.com>
;; Maintainer: Tim Visher <tim.visher@gmail.com>
;; Created: 2011-11-02
;; Version: 0.1
;; Package-Version: 6a616502
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

(defgroup vimgolf nil
  "Compete on VimGolf with the One True Editor."
  :prefix "vimgolf-"
  :group 'applications)

(defcustom vimgolf-key nil
  "Your VimGolf API Key. Must be set in order to submit your solution."
  :type 'string
  :group 'vimgolf)

(defcustom vimgolf-mode-hook '((lambda () (whitespace-mode t)))
  "A list of functions to call upon the initialization of vimgolf-mode."
  :type 'hook
  :group 'vimgolf)

(defvar vimgolf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v C-c") 'vimgolf-submit)
    (define-key map (kbd "C-c C-v r") 'vimgolf-revert)
    (define-key map (kbd "C-c C-v d") 'vimgolf-diff)
    (define-key map (kbd "C-c C-v c") 'vimgolf-continue)
    (define-key map (kbd "C-c C-v p") 'vimgolf-pause)
    (define-key map (kbd "C-c C-v q") 'vimgolf-quit)
    map))

(define-minor-mode vimgolf-mode
  "Toggle VimGolf mode.

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode.

When VimGolf mode is enabled, several key bindings are defined
with `C-c C-v` prefixes to help in playing VimGolf.

\\{vimgolf-mode-map}"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " VimGolf"
  ;; The minor mode bindings.
  :keymap vimgolf-mode-map
  :group 'vimgolf)

(defvar vimgolf-challenge nil)

(defvar vimgolf-prior-window-configuration nil)

(defvar vimgolf-working-window-configuration nil)

(defvar vimgolf-work-buffer-name "*vimgolf-work*")
(defvar vimgolf-start-buffer-name "*vimgolf-start*")
(defvar vimgolf-end-buffer-name "*vimgolf-end*")
(defvar vimgolf-keystrokes-buffer-name "*vimgolf-keystrokes*")
(defvar vimgolf-keystrokes-log-buffer-name "*vimgolf-keystrokes-log*")

(defun vimgolf-solution-correct-p ()
  "Return t if the work text is identical to the solution, nil otherwise."
  (let ((case-fold-search nil)
        (work vimgolf-work-buffer-name)
        (end vimgolf-end-buffer-name))
    (flet ((point-min-in (buf) (with-current-buffer buf (point-min)))
           (point-max-in (buf) (with-current-buffer buf (point-max))))
      (zerop (compare-buffer-substrings
              (get-buffer work) (point-min-in work) (point-max-in work)
              (get-buffer end) (point-min-in end) (point-max-in end))))))

(defun vimgolf-wrong-solution ()
  (message "Wrong!")
  (vimgolf-diff))

(defmacro vimgolf-with-saved-command-environment (&rest body)
  `(let ((deactivate-mark nil)
         (this-command this-command)
         (last-command last-command))
     ,@body))

(defun vimgolf-capturable-keystroke-p ()
  (not (or executing-kbd-macro
           (< 0 (recursion-depth))
           (member this-command
                   '(digit-argument
                     negative-argument
                     universal-argument
                     universal-argument-other-key
                     universal-argument-minus
                     universal-argument-more))
           (string-prefix-p "vimgolf-" (symbol-name this-command)))))

(defun vimgolf-capturable-dangling-keystroke-p ()
  (member this-command
          '(calc-dispatch)))

(defun vimgolf-capture-keystroke ()
  (vimgolf-with-saved-command-environment
   (when (vimgolf-capturable-keystroke-p)
     (with-current-buffer (get-buffer-create vimgolf-keystrokes-buffer-name)
       (end-of-buffer)
       (insert (key-description (this-command-keys)))
       (insert " ")))))

(defun vimgolf-capture-dangling-keystroke ()
  (vimgolf-with-saved-command-environment
   (when (vimgolf-capturable-dangling-keystroke-p)
     (with-current-buffer (get-buffer-create vimgolf-keystrokes-buffer-name)
       (end-of-buffer)
       (insert (key-description (this-command-keys)))
       (insert " ")))))

;; (setq vimgolf-logging-enabled t)
;; (setq vimgolf-logging-enabled)
(defvar vimgolf-logging-enabled nil)

(defun vimgolf-log-keystroke ()
  (when (and vimgolf-logging-enabled (not (< 0 (recursion-depth))))
    (vimgolf-with-saved-command-environment
     (with-current-buffer (get-buffer-create vimgolf-keystrokes-log-buffer-name)
       (end-of-buffer)
       (insert (key-description (this-command-keys)))
       (insert " ")
       (princ this-command (get-buffer-create vimgolf-keystrokes-log-buffer-name))
       (insert "\n")))))

(defun vimgolf-enable-capture (enable)
  "Enable keystroke logging if `ENABLE' is non-nil otherwise disable it."
  (let ((f (if enable 'add-hook 'remove-hook)))
    (funcall f 'pre-command-hook 'vimgolf-capture-keystroke)
    (funcall f 'post-command-hook 'vimgolf-capture-dangling-keystroke)
    (funcall f 'pre-command-hook 'vimgolf-log-keystroke)
    (funcall f 'post-command-hook 'vimgolf-log-keystroke)))

(defun vimgolf-count-keystrokes ()
  (with-current-buffer (get-buffer vimgolf-keystrokes-buffer-name)
    (beginning-of-buffer)
    (let ((count 0))
      (while (search-forward " " nil t)
        (setq count (1+ count)))
      count)))

(defun vimgolf-right-solution ()
  (message "Hurray!")
  (let ((keystrokes-count (vimgolf-count-keystrokes)))        ; Need to implement keystroke counting. Should be as simple as counting spaces and adding 1.
    (message "You solved %s in %s keystrokes!" vimgolf-challenge keystrokes-count)))

(defun vimgolf-submit ()
  "Stop the challenge and attempt to submit the solution to VimGolf."
  (interactive)
  (vimgolf-enable-capture nil)
  (if (vimgolf-solution-correct-p) (vimgolf-right-solution) (vimgolf-wrong-solution)))

(defun vimgolf-clear-keystrokes ()
  (with-current-buffer (get-buffer-create vimgolf-keystrokes-buffer-name)
    (erase-buffer)))

(defun vimgolf-reset-work-buffer ()
  "Reset the contents of the work buffer, and clear undo/macro history etc."
  (with-current-buffer (get-buffer-create vimgolf-work-buffer-name)
    (vimgolf-init-buffer (current-buffer)
                         (with-current-buffer vimgolf-start-buffer-name
                           (buffer-string)))
    (when defining-kbd-macro
      (end-kbd-macro))
    (vimgolf-clear-keystrokes)
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil)))

(defun vimgolf-revert ()
  "Revert the work buffer to it's original state and reset keystrokes."
  (interactive)
  (vimgolf-reset-work-buffer)
  (set-window-configuration vimgolf-working-window-configuration)
  (message "If at first you don't succeed, try, try again."))

(defun vimgolf-diff ()
  "Pause the competition and view differences between the buffers."
  (interactive)
  (vimgolf-enable-capture nil)
  (ediff-buffers (get-buffer-create vimgolf-work-buffer-name) (get-buffer-create vimgolf-end-buffer-name))
  (message "Remember to `C-c C-v c` when you're done."))

(defun vimgolf-continue ()
  "Restore work and end buffers and begin recording keystrokes again."
  (interactive)
  (vimgolf-enable-capture t)
  (set-window-configuration vimgolf-working-window-configuration)
  (message "Golf away!"))

(defun vimgolf-pause ()
  "Stop recording keystrokes."
  (interactive)
  (vimgolf-enable-capture nil)
  (message "Come `C-c C-v c` soon."))

(defun vimgolf-quit ()
  "Cancel the competition."
  (interactive)
  (if (get-buffer vimgolf-start-buffer-name) (kill-buffer vimgolf-start-buffer-name))
  (if (get-buffer vimgolf-work-buffer-name) (kill-buffer vimgolf-work-buffer-name))
  (if (get-buffer vimgolf-end-buffer-name) (kill-buffer vimgolf-end-buffer-name))
  (vimgolf-enable-capture nil)
  (set-window-configuration vimgolf-prior-window-configuration)
  (message "I declare you, n00b!"))

(defvar vimgolf-host "http://vimgolf.com/")

;; (setq vimgolf-host "http://vimgolf.local:8888/")
;; (setq vimgolf-host "http://vimgolf.com/")
;; Overall VimGolf Rank ID: 4d2fb20e63b08b08b0000075
;; Sort entries based on date ID: 4ea9bc988b36f70001000008
;; HTML to Haml ID: 4d3c51f1aabf526ed6000030

(defvar vimgolf-challenge-extension ".yaml")

(defun vimgolf-challenge-path (challenge-id)
  (concat "challenges/" challenge-id))

(defun vimgolf-challenge-url (challenge-id)
  (concat vimgolf-host (vimgolf-challenge-path challenge-id) vimgolf-challenge-extension))

(defun vimgolf-init-buffer (buffer text)
  (with-current-buffer buffer
    (erase-buffer)
    (insert text)
    (beginning-of-buffer)
    (vimgolf-mode t)))

(defun vimgolf-kill-existing-session ()
  (dolist (buf (list vimgolf-start-buffer-name
                     vimgolf-work-buffer-name
                     vimgolf-end-buffer-name
                     vimgolf-keystrokes-buffer-name
                     vimgolf-keystrokes-log-buffer-name))
    (when (get-buffer buf)
      (kill-buffer buf))))

(defun vimgolf-read-next-data-chunk ()
  "Return the next chunk of data as a string, leaving the point at the end of that chunk."
  (let ((data-start-regexp "  data: |\\+\\{0,1\\}\n")
        (data-end-regexp "\\([ 	]\\{4\\}\\|[ 	]\\{0\\}\\)\n  type: [-a-z]+"))
    (unless (re-search-forward data-start-regexp nil t)
      (error "Can't find data in response from vimgolf"))
    (let ((start (point)))
      (unless (re-search-forward data-end-regexp nil t)
        (error "Unclosed data section in response from vimgolf"))
      (let ((str (buffer-substring-no-properties start (match-beginning 0))))
        (replace-regexp-in-string "^    " "" str)))))

;;;###autoload
(defun vimgolf (challenge-id)
  "Open a VimGolf Challenge"
  (interactive "sChallenge ID: ")
  (vimgolf-clear-keystrokes)
  (setq vimgolf-prior-window-configuration (current-window-configuration)
        vimgolf-challenge challenge-id)
  (let ((vimgolf-yaml-buffer (url-retrieve-synchronously (vimgolf-challenge-url challenge-id))))
    (set-buffer vimgolf-yaml-buffer)
    (beginning-of-buffer)
    (let* ((start-text (vimgolf-read-next-data-chunk))
           (end-text (vimgolf-read-next-data-chunk)))

      (vimgolf-kill-existing-session)

      (let ((vimgolf-start-buffer (get-buffer-create vimgolf-start-buffer-name))
            (vimgolf-work-buffer (get-buffer-create vimgolf-work-buffer-name))
            (vimgolf-end-buffer (get-buffer-create vimgolf-end-buffer-name)))

        (vimgolf-init-buffer vimgolf-start-buffer start-text)
        (vimgolf-init-buffer vimgolf-end-buffer end-text)
        (vimgolf-reset-work-buffer)

        ;; Set up windows
        (delete-other-windows)
        (display-buffer vimgolf-end-buffer 'display-buffer-pop-up-window)
        (set-window-buffer (selected-window) vimgolf-work-buffer)
        (switch-to-buffer vimgolf-work-buffer)
        (setq vimgolf-working-window-configuration (current-window-configuration))

        (vimgolf-enable-capture t)))))

;;; vimgolf.el ends here
