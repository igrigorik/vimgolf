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
                   "I know it's possible to use #. in pandoc to auto-generate numbered lists, but then it's not easy to tell how many items there are when reading it in Markdown. How fast can you make the switch?")
                  ("4fe62f8a8b2f800001000043"
                   "Cleanining up 80 column concatenated text"
                   "1. Put the whole query on one line, remove the concatenation operators2. Remove the double spaces that appear a few times in the query3. Use string-interpolated variable statementPid rather than the ugly concatenation")
                  ("4d1b4ac3c58eaa2a8a0005c2"
                   "Ruby 1.9 compat"
                   "Remember when Ruby supported `when &lt;expr&gt; :`? Well, it doesn't in 1.9, so let's make sure we use `then`, without ruining our lovely new hash syntax!")
                  ))
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
