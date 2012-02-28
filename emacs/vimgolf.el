;;; vimgolf.el --- VimGolf interface for the One True Editor

;; Copyright (C) never, by no one

;;; Author: Tim Visher <tim.visher@gmail.com>
;;; Maintainer: Tim Visher <tim.visher@gmail.com>
;;; Created: 2011-11-02
;;; Version: 0.9.2
;;; Keywords: games vimgolf vim

;; This file is not part of GNU Emacs

;;; Commentary:

;; This is a simple package that allows Emacs users to compete on [VimGolf][1]
;; using the One True Editor. Competition can be commenced utilizing `M-x
;; vimgolf`. When finished with a challenge, `C-c C-v C-c` should finish your
;; editing, ensure correctness, and submit your score and keystrokes to
;; [VimGolf][1].
;;
;; On second thought, let's not go to Camelot. It's a silly place.
;;
;; Patches are accepted at https://github.com/timvisher/vimgolf
;;
;; [1]: http://vimgolf.com/

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

;;; Contributors

;; Tim Visher (@timvisher)
;; Steve Purcell (@sanityinc)
;; Adam Collard (@acollard)

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
(defvar vimgolf-challenge-history nil)

(defvar vimgolf-prior-window-configuration nil)

(defvar vimgolf-working-window-configuration nil)

(defvar vimgolf-work-buffer-name "*vimgolf-work*")
(defvar vimgolf-start-buffer-name "*vimgolf-start*")
(defvar vimgolf-end-buffer-name "*vimgolf-end*")
(defvar vimgolf-keystrokes-buffer-name "*vimgolf-keystrokes*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keystroke logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro vimgolf-with-saved-command-environment (&rest body)
  `(let ((deactivate-mark nil)
         (this-command this-command)
         (last-command last-command))
     ,@body))

(defun vimgolf-capturable-keystroke-p ()
  (not (or executing-kbd-macro
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

(defvar vimgolf-keystrokes nil
  "A list of (keys-vector . command) pairs for the keystrokes entered.

Each entry is a cons cell containing a key sequence vector
suitable for use with `key-description', and a symbol for the
command that was executed as a result (which may be nil if an
unknown key sequence was entered).")

(defun vimgolf-maybe-capture-keystroke (pred)
  "Store the keystrokes for `this-command' if result of calling function `PRED' is not nil."
  (vimgolf-with-saved-command-environment
   (when (funcall pred)
     (setq vimgolf-keystrokes
           (append vimgolf-keystrokes (list (cons (this-command-keys-vector)
                                                  this-command)))))))

(defun vimgolf-capture-keystroke ()
  (vimgolf-maybe-capture-keystroke 'vimgolf-capturable-keystroke-p))

(defun vimgolf-capture-dangling-keystroke ()
  (vimgolf-maybe-capture-keystroke 'vimgolf-capturable-dangling-keystroke-p))

(defun vimgolf-refresh-keystroke-log ()
  "Refresh the contents of the keystrokes log buffer."
  (let ((deactivate-mark nil))
    (with-current-buffer (get-buffer-create vimgolf-keystrokes-buffer-name)
      (vimgolf-mode t)
      (erase-buffer)
      (insert (format "Challenge ID: %s\n%s\n\n" vimgolf-challenge (vimgolf-challenge-url vimgolf-challenge))
              (format "Keystrokes (%d):\n\n" (vimgolf-count-keystrokes))
              (mapconcat 'key-description (mapcar 'car vimgolf-keystrokes) " ")
              "\n\nFull command log:\n\n")
      (when vimgolf-keystrokes
        (let* ((descrs-and-commands
                (mapcar (lambda (entry) (cons (key-description (car entry)) (cdr entry))) vimgolf-keystrokes))
               (maxlen (apply 'max (mapcar 'length (mapcar 'car descrs-and-commands))))
               (fmt (format "%%-%ds  %%s" maxlen)))
          (dolist (entry descrs-and-commands)
            (insert (format fmt (car entry) (prin1-to-string (cdr entry) t)) "\n")))))))

(defun vimgolf-enable-capture (enable)
  "Enable keystroke logging if `ENABLE' is non-nil otherwise disable it."
  (let ((f (if enable 'add-hook 'remove-hook)))
    (funcall f 'pre-command-hook 'vimgolf-capture-keystroke)
    (funcall f 'post-command-hook 'vimgolf-capture-dangling-keystroke)
    (funcall f 'post-command-hook 'vimgolf-refresh-keystroke-log)))

(defun vimgolf-count-keystrokes ()
  (apply '+ (mapcar 'length (mapcar 'car vimgolf-keystrokes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing and scoring challenges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimgolf-solution-correct-p ()
  "Return t if the work text is identical to the solution, nil otherwise."
  (let ((working (with-current-buffer vimgolf-work-buffer-name (buffer-string)))
        (end (with-current-buffer vimgolf-end-buffer-name (buffer-string))))
    (string= working end)))

(defun vimgolf-wrong-solution ()
  (message "Wrong!")
  (vimgolf-diff))

(defun vimgolf-right-solution ()
  (delete-other-windows)
  (switch-to-buffer vimgolf-keystrokes-buffer-name)
  (message "Hurray! You solved %s in %d keystrokes!" vimgolf-challenge (vimgolf-count-keystrokes)))

(defun vimgolf-submit ()
  "Stop the challenge and attempt to submit the solution to VimGolf."
  (interactive)
  (vimgolf-enable-capture nil)
  (if (vimgolf-solution-correct-p) (vimgolf-right-solution) (vimgolf-wrong-solution)))

(defun vimgolf-clear-keystrokes ()
  (setq vimgolf-keystrokes nil))

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
  (vimgolf-enable-capture nil)
  (vimgolf-kill-existing-session)
  (set-window-configuration vimgolf-prior-window-configuration)
  (message "I declare you, n00b!"))

(defvar vimgolf-host "http://vimgolf.com/")

;; (setq vimgolf-host "http://vimgolf.local:8888/")
;; (setq vimgolf-host "http://vimgolf.com/")
;; Overall VimGolf Rank ID: 4d2fb20e63b08b08b0000075
;; Sort entries based on date ID: 4ea9bc988b36f70001000008
;; HTML to Haml ID: 4d3c51f1aabf526ed6000030
;; Assignment Allignment: 4d2c9d06eda6262e4e00007a

(defvar vimgolf-challenge-extension ".yaml")

(defun vimgolf-challenge-path (challenge-id)
  (concat "challenges/" challenge-id))

(defun vimgolf-challenge-url (challenge-id)
  (concat vimgolf-host (vimgolf-challenge-path challenge-id) vimgolf-challenge-extension))

(defun vimgolf-init-buffer (buffer text)
  (with-current-buffer buffer
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (vimgolf-mode t)))

(defun vimgolf-kill-existing-session ()
  "Kill any vimgolf-related buffers."
  (dolist (buf (list vimgolf-start-buffer-name
                     vimgolf-work-buffer-name
                     vimgolf-end-buffer-name
                     vimgolf-keystrokes-buffer-name))
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

(defun vimgolf-setup (status challenge-id)
  (vimgolf-clear-keystrokes)
  (setq vimgolf-prior-window-configuration (current-window-configuration)
        vimgolf-challenge challenge-id)
  (goto-char (point-min))
  (let* ((start-text (vimgolf-read-next-data-chunk))
         (end-text (vimgolf-read-next-data-chunk)))

    (vimgolf-kill-existing-session)

    (let ((vimgolf-start-buffer (get-buffer-create vimgolf-start-buffer-name))
          (vimgolf-work-buffer (get-buffer-create vimgolf-work-buffer-name))
          (vimgolf-end-buffer (get-buffer-create vimgolf-end-buffer-name)))

      (vimgolf-init-buffer vimgolf-start-buffer start-text)
      (vimgolf-init-buffer vimgolf-end-buffer end-text)
      (with-current-buffer vimgolf-end-buffer (setq buffer-read-only t))
      (vimgolf-reset-work-buffer)

      ;; Set up windows
      (delete-other-windows)
      (display-buffer vimgolf-end-buffer 'display-buffer-pop-up-window)
      (set-window-buffer (selected-window) vimgolf-work-buffer)
      (switch-to-buffer vimgolf-work-buffer)
      (setq vimgolf-working-window-configuration (current-window-configuration))

      (vimgolf-continue))))

;;;###autoload
(defun vimgolf (challenge-id)
  "Open a VimGolf Challenge"
  (interactive (list (read-from-minibuffer "Challenge ID: " nil nil nil 'vimgolf-challenge-history)))
  (url-retrieve (vimgolf-challenge-url challenge-id) 'vimgolf-setup `(,challenge-id)))

(provide 'vimgolf)

;;; Local Variables:
;;; tab-width:2
;;; indent-tabs-mode:nil
;;; End:
;;; vimgolf.el ends here
