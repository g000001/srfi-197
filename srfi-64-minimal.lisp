;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "https://github.com/g000001/srfi-197#internals")

; Just enough of SRFI 64 (unit tests) to run test.scm.

(defvar *test-failures* '())

(defun test-reset ()
  (setq *test-failures* '()))

(defun test-begin (name)
  (terpri)
  (princ "Test group: ")
  (princ name)
  (terpri)
  (terpri))

(defun test-end (name)
  (declare (ignore name))
  (terpri)
  (cond
    ((null *test-failures*)
      (princ "All tests passed!")
      (terpri)
      (terpri)
      (print 'ok))
    (:else
      (write (length *test-failures*))
      (princ " TEST(S) FAILED:")
      (terpri)
      (mapc (lambda (x) (funcall x)) (reverse *test-failures*))
      (terpri)
      (print 'ng))))

(defun test-equal (name expected actual)
  (cond
    ((equal expected actual)
      (princ "PASS: "))
    (:else
      (setq *test-failures*
            (cons
              (lambda ()
                (princ name)
                (princ ": Expected ")
                (write expected)
                (princ ", got ")
                (write actual)
                (terpri))
              *test-failures*))
      (princ "FAIL: ")))
  (princ name)
  (terpri))


(defmacro test-skip (name expected actual)
  (declare (ignore expected actual))
  (princ "  SKIP: ")
  (princ name)
  (terpri))


