;;; Notes:

;; This is the old way to call emacs scripts
; :;exec emacs -batch -l "$0" -f main -- "$@"            
(defun example ()
  (princ (format "%d\n" (+ 1 2))))

;; Printing arguments back out.  Send in strings like this:
;; ./scriptname \"ABC\" def ghk
;;
;; - We don't use args for the script I developed, but
;; it's useful for reference
(princ (format "args: %s\n" command-line-args-left))

;; Errors/kills
(condition-case nil (check-parens) (error (kill-emacs 1)))

;; Sending different error codes
(kill-emacs 113)

;; Examine the output of that with:  echo $?  in bash
