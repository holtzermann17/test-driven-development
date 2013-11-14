;;; Pass in an increment:

;; This is trickier, because the increment has to be sent
;; as an argument.  I guess I can create a fixture that
;; allows the user to interactively supply the name of a
;; buffer to test, for example.
;;
;; OK, this code all works, but I'm pulling it out of the
;; main test-driven-development.el file, b/c the
;; interaction would be annoying when I'm testing.
;;
;; Later code uses a pattern like the one established
;; here.

(defun FACE-select-buffer-to-count-fixture (body)
  (let ((buffer (read-buffer "Buffer to scan: ")))
    (unwind-protect (progn 
                     (save-excursion (set-buffer (get-buffer buffer))
                                      (funcall body)))
      )))

; A test that makes use of the fixture described above
(ert-deftest FACE-select-buffer-and-count-test ()
  (FACE-select-buffer-to-count-fixture
   (lambda ()
     (let ((number-of-defuns (count-matches "^(defun" (point-min) (point-max))))
      (should (equal number-of-defuns 0))))))
