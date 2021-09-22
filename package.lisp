;;;; package.lisp

(cl:in-package cl-user)


(defpackage "https://github.com/g000001/srfi-197"
  (:use)
  (:export chain chain-and chain-when chain-lambda nest nest-reverse))
  

(defpackage "https://github.com/g000001/srfi-197#internals"
  (:use
   "https://github.com/g000001/srfi-197"
   "https://github.com/g000001/srfi-2"
   "https://github.com/g000001/srfi-11"
   cl
   "https://github.com/g000001/srfi-46")
  (:export srfi-197-syntax))


;;; *EOF*
