;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I open the vimgolf browser$"
       (lambda ()
         (setq *vimgolf-browse-list*
                '(("50128129201f450002000027"
                   "Complete the circuit grid!"
                   "Continuing from the last challenge, add additional rows to the grid. Change each of the names from 'A1'...'A10' to 'B1'...'B10', 'C1'...'C10', etc, and for each new row add 0.7 to the X values in the (X Y) at the end of each line. ")
                  ("50127eba201f450002000024"
                   "Make the circuit grid!"
                   "Copy this command for a circuit layout program to create 10 total smd commands. Increment the number in quotes to name each pad, and add 0.7 to each of the (X Y) coordinates at the end of each line.")
                  ("500855e60599d90002000073"
                   "Convert pandoc unordered list to a numbered list"
                   "I know it's possible to use #. in pandoc to auto-generate numbered lists, but then it's not easy to tell how many items there are when reading it in Markdown. How fast can you make the switch?")))
         (vimgolf-browse)))

(When "^I show the description$"
      (lambda ()
        (vimgolf-show-description)))

(Given "^I have \"\\(.+\\)\"$"
       (lambda (something)
         ;; Do something
         ))

(When "^I have \"\\(.+\\)\"$"
      (lambda (something)
        ;; Do something
        ))

(Then "^I should have \"\\(.+\\)\"$"
      (lambda (something)
        ;; Do something
        ))

(And "^I have \"\\(.+\\)\"$"
     (lambda (something)
       ;; Do something
       ))

(But "^I should not have \"\\(.+\\)\"$"
     (lambda (something)
       ;; Do something
       ))
