;;; test-driven-development.el - sketch use of the FACE model to record creative progress

;;; Documentation: See README.md

;;; Code:

(require 'ert)
(require 'ert-x)

;;; Schema for main kinds of tests that we're interested in

;;; Basic versions

;; These tests should take an "increment" as an argument.
;; We'll have to come up with a way to pass in a buffer or
;; string to the test, perhaps using a fixture (see
;; below).

(ert-deftest FACE-new-framing-produced ()
  "Test whether a new framing exists now (for some definition of \"framing\").

Hint: This would look for the existence of new documentation, for instance."
  ;(should (= (+ 1 2) 4))
  )

(ert-deftest FACE-new-aesthetic-produced ()
  "Test whether a new aesthetic exists now (for some definition of \"aesthetic\").

Hint: \"Sophisticated\" (but elementary) definition: A new map into [0,1] exists.

Example: this concept is interesting, but it needs:
- an example
- a picture
- a relationship to another concept in the domain
- a conjecture
etc."
  ;; jac - Note that even more complex version would allow a map to another manifold.
  ;(should (= (+ 1 2) 4))
  )

(ert-deftest FACE-new-concept-produced ()
  "Test whether a new concept exists now (for some definition of \"concept\").

Hint: Naive version:  I have a database of facts and I've added something to it
Sophisticated version:  I have a collection of input/output functions and
I've added something to it"
  ;(should (= (+ 1 2) 4))
  )

(ert-deftest FACE-new-example-produced ()
  "Test whether a new example exists now (for some definition of \"example\").

Hint: Example or instantiation of a concept.  (Every example is
also an \"expression\" of a concept.)

Concept : prime factorization (best case, this is embodied
in a program for doing prime factorization)

Example: 14 -> 7*2

Some examples of examples:  

- a mathematics problem or exercise (the problem \"begs\" the example)
- an application"
  ;(should (= (+ 1 2) 4))
  )

;;; `method' versions: at least on the surface, this is harder

(ert-deftest FACE-new-framing-method-produced ()
  "Test whether a new method for framing exists now (for some definition of \"framing\").

Hint: This might look for the existence of new technique for generating documentation,
for instance."
  ;(should (= (+ 1 2) 4))
  )

(ert-deftest FACE-new-aesthetic-method-produced ()
  "Test whether a new method for making an aesthetic exists now (for some definition of \"aesthetic\").

Hint: all this is, is a new way of computing a fitness function!"
  ;; jac Nov 7, 2013 - Maybe there's something about survival analysis that could added in here.
  ;(should (= (+ 1 2) 4))
  )

(ert-deftest FACE-new-concept-method-produced ()
  "Test whether a new method for making a concept exists now (for some definition of \"concept\").

Hint: Naive version:  I have a new routine for adding things to my database (e.g.
Mike's system can look something up online). Sophisticated version:  I have a new
macro that writes functions."
  ;(should (= (+ 1 2) 4))
  )

(ert-deftest FACE-new-example-method-produced ()
  "Test whether a new method for making an example exists now (for some definition of \"example\").

Hint: Euler's summation to pi^2 / 6 is a proof, but also an illustration
of a method for doing other proofs."
  ;(should (= (+ 1 2) 4))
  )


;;; Implement a cheap counting test:

;; The first test here "fails" whenever a new
;; `ert-deftest' has been defined, and produces the actual
;; number of new `ert-deftest's as part of the
;; explanation, which is what I am going for.  I define
;; similar counting tests for the number of defuns and
;; number of defvars etc. below.

(ert-deftest FACE-count-tests ()
  (save-excursion 
    (set-buffer (get-buffer FACE-buffer-as-increment))
    (let ((number-of-tests (count-matches "^(ert-deftest" (point-min) (point-max))))
      (should (equal number-of-tests 0)))))

(ert-deftest FACE-count-defuns ()
  (save-excursion
    (set-buffer (get-buffer FACE-buffer-as-increment))
    (let ((number-of-defuns (count-matches "^(defun" (point-min) (point-max))))
      (should (equal number-of-defuns 0)))))

(ert-deftest FACE-count-defvars ()
  (save-excursion
    (set-buffer (get-buffer FACE-buffer-as-increment))
    (let ((number-of-variables (count-matches "^(defvar" (point-min) (point-max))))
      (should (equal number-of-variables 0)))))

(ert-deftest FACE-count-defns ()
  (save-excursion
    (set-buffer (get-buffer FACE-buffer-as-increment))
    (let ((number-of-variables (count-matches "^(defn" (point-min) (point-max))))
      (should (equal number-of-variables 0)))))

(ert-deftest FACE-count-defthm ()
  (save-excursion
    (set-buffer (get-buffer FACE-buffer-as-increment))
    (let ((number-of-variables (count-matches "^(defn" (point-min) (point-max))))
      (should (equal number-of-variables 0)))))

;;; Run a battery of tests on a given file:

;; All I really need is a function similar to
;; `ert-run-tests-interactively', but that takes an
;; argument, and makes that available to the various tests
;; before the function runs.  That's easy.  Later we'll
;; want a version that knows where to look for the
;; increment, and just produces a report.

(defvar FACE-buffer-as-increment nil)

(defun FACE-select-buffer-and-run-tests (buffer)
  "Provides is an interactive way to run tests on contents of BUFFER."
  (interactive "b")
  (setq FACE-buffer-as-increment buffer)
  (ert-run-tests-interactively "^FACE"))

;;; End of file
