;;;; srfi-197.asd

(cl:in-package :asdf)


(defsystem :srfi-197
  :version "20210917"
  :description "SRFI 197 for CL: Pipeline Operators"
  :long-description "SRFI 197 for CL: Pipeline Operators
https://srfi.schemers.org/srfi-197"
  :author "Adam Nelson"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (srfi-2 srfi-11 srfi-46)
  :components ((:file "package")
               (:file "srfi-197")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-197))))
  (let ((name "https://github.com/g000001/srfi-197")
        (nickname :srfi-197))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-197))))
  (let ((dir (system-source-directory :srfi-197)))
    (load (merge-pathnames #P"srfi-64-minimal.lisp" dir))
    (load (merge-pathnames #P"test.lisp" dir))))


;;; *EOF*
