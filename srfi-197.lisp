;;;; srfi-88.lisp

(cl:in-package "https://github.com/g000001/srfi-197#internals")

(defconstant |#f| '|#f|)


(defmacro define-syntax* (name &body body)
  (let ((*name (gensym (string name))))
    `(progn
       (define-syntax ,*name ,@body)
       (defmacro ,name (&body body)
         `(,',*name ,@(subst-if '_ (lambda (x) 
                                     (typecase x
                                       (null nil)
                                       (symbol (member-if (lambda (y)
                                                            (string= x y))
                                                          '(_ ***)))
                                       (T nil)))
                                body))))))

(define-syntax %chain
  (syntax-rules … (_ ***)
    ; (_ in-step out-step in-vars out-vars in-steps out-steps)
    ((%chain () ;in-step
             () ;out-step
             () ;in-vars
             ((var) …) ;out-vars
             () ;in-steps
             (step … last-step) ;out-steps
             )
      (let* ((var step) …) last-step))
    ((%chain () () () (vars …) () (step … last-step))
      (let*-values ((vars step) …) last-step))
    ((%chain () () () out-vars (next-step . in-steps) out-steps)
      (%chain next-step () () out-vars in-steps out-steps))
    ((%chain () step () (out-vars …) in-steps (out-steps …))
      (%chain () () () (out-vars … ignored) in-steps (out-steps … step)))
    ((%chain () step vars (out-vars …) in-steps (out-steps …))
      (%chain () () () (out-vars … vars) in-steps (out-steps … step)))
    ((%chain (_ ***) (step …) () (out-vars …) in-steps (out-steps …))
      (%chain () () () (out-vars … chain-rest-var) in-steps (out-steps … (apply step … chain-rest-var))))
    ((%chain (_ ***) (step …) (vars …) (out-vars …) in-steps (out-steps …))
      (%chain () () () (out-vars … (vars … . chain-rest-var)) in-steps (out-steps … (apply step … chain-rest-var))))
    ((%chain (_ *** . rest) . _)
      (syntax-error "_ *** can only be used as a final argument"))
    ((%chain (_ . in-step) (out-step …) (vars …) . rest)
      (%chain in-step (out-step … chain-var) (vars … chain-var) . rest))
    ((%chain (x . in-step) (out-step …) . rest)
      (%chain in-step (out-step … x) . rest))))

;;FIXME
(defmacro %%chain ((var initial-value) &rest steps)
  `(let* ((,var ,initial-value)
          ,@(mapcar (lambda (s)
                      `(,var ,s))
                    steps))
     ,var))

(define-syntax* chain
  (syntax-rules …  ()
    ((_ initial-value) initial-value)
    ((_ initial-value (first-step …) (step …) …)
      (%chain (first-step …) () () () ((step …) …) (initial-value)))
    ((_ initial-value first-step (step …) …)
      (%%chain (first-step initial-value) (step …) …))))

(define-syntax %chain-and
  (syntax-rules … (_)
    ; (_ in-step out-step in-vars out-vars in-steps out-steps)
    ((%chain-and () () () (var …) () (step … last-step))
      (and-let* ((var step) …) last-step))
    ((%chain-and () () () out-vars (next-step . in-steps) out-steps)
      (%chain-and next-step () () out-vars in-steps out-steps))
    ((%chain-and () step () (out-vars …) in-steps (out-steps …))
      (%chain-and () () () (out-vars … ignored) in-steps (out-steps … step)))
    ((%chain-and () step (var) (out-vars …) in-steps (out-steps …))
      (%chain-and () () () (out-vars … var) in-steps (out-steps … step)))
    ((%chain-and (_ . in-step) (out-step …) () . rest)
      (%chain-and in-step (out-step … chain-var) (chain-var) . rest))
    ((%chain-and (_ . excess) . rest)
      (syntax-error "chain-and does not support multiple _ in a single step"))
    ((%chain-and (x . in-step) (out-step …) . rest)
      (%chain-and in-step (out-step … x) . rest))))

(define-syntax* chain-and
  (syntax-rules … ()
    ((_ initial-value) initial-value)
    ((_ initial-value (first-step …) (step …) …)
      (%chain-and (first-step …) () () () ((step …) …) (initial-value)))))

(define-syntax %chain-when
  (syntax-rules … (_ |#f|)
    ; (_ in-step out-step guard? chain-var in-steps out-expr)
    ((%chain-when () () _1 _2 () out-expr) out-expr)
    ((%chain-when () () _1 _2 ((next-guard? next-step) . in-steps) out-expr)
      (%chain-when next-step () next-guard? |#f| in-steps out-expr))
    ((%chain-when () step guard? |#f| in-steps out-expr)
      (%chain-when () () |#f| |#f| in-steps
        (let ((chain-var out-expr))
          (if guard? step chain-var))))
    ((%chain-when () step guard? chain-var in-steps out-expr)
      (%chain-when () () |#f| |#f| in-steps
        (let ((chain-var out-expr))
          (if guard? step chain-var))))
    ((%chain-when (_ . in-step) (out-step …) guard? |#f| . rest)
      (%chain-when in-step (out-step … chain-var) guard? chain-var . rest))
    ((%chain-when (_ . excess) . rest)
      (syntax-error "chain-when does not support multiple _ in a single step"))
    ((%chain-when (x . in-step) (out-step …) . rest)
      (%chain-when in-step (out-step … x) . rest))))

(define-syntax* chain-when
  (syntax-rules … (|#f|)
    ((_ initial-value) initial-value)
    ((_ initial-value (first-guard? (first-step …)) (guard? (step …)) …)
     (%chain-when (first-step …) () first-guard? |#f| ((guard? (step …)) …) initial-value))))

(define-syntax %chain-lambda
  (syntax-rules … (_ ***)
    ; (_ in-step out-step args rest-of-steps)
    ((%chain-lambda () first-step args steps)
      (lambda args (chain first-step . steps)))
    ((%chain-lambda (_ ***) (step …) () steps)
      (lambda chain-rest-var (chain (apply step … chain-rest-var) . steps)))
    ((%chain-lambda (_ ***) (step …) (args …) steps)
      (lambda (args … . chain-rest-var) (chain (apply step … chain-rest-var) . steps)))
    ((%chain-lambda (_ *** . excess) . rest)
      (syntax-error "_ *** can only be used as a final argument"))
    ((%chain-lambda (_ . in-step) (out-step …) (args …) . rest)
      (%chain-lambda in-step (out-step … chain-var) (args … chain-var) . rest))
    ((%chain-lambda (x . in-step) (out-step …) . rest)
      (%chain-lambda in-step (out-step … x) . rest))))

(define-syntax* chain-lambda
  (syntax-rules … ()
    ((_ (first-step …) (step …) …)
      (%chain-lambda (first-step …) () () ((step …) …)))))

(define-syntax* nest
  (syntax-rules … (_)
    ((nest last) last)
    ((nest (step …) … last) (nest _ (step …) … last))
    ((nest placeholder (extra-step …) … (first-step …) last)
      (let ()
        ; let-syntax is buggy in some Schemes, define-syntax is more reliable
        (define-syntax %nest
          (syntax-rules (placeholder)
            ((%nest result () placeholder ()) result)
            ((%nest result () placeholder (rest *** step))
              (%nest () step result (rest ***)))
            ((%nest result () accum steps)
              (syntax-error "nest: step must contain _"))
            ((%nest result (placeholder . rest) placeholder steps)
              (syntax-error "nest: only one _ allowed per step"))
            ((%nest (result ***) (placeholder . rest) accum steps)
              (%nest (result *** accum) rest placeholder steps))
            ((%nest (result ***) (element . rest) accum steps)
              (%nest (result *** element) rest accum steps))))
        (%nest () (first-step …) last ((extra-step …) …))))
    ((nest placeholder last) last)))

(define-syntax* nest-reverse
  (syntax-rules … (_)
    ((nest-reverse first) first)
    ((nest-reverse first (step …) …) (nest-reverse first _ (step …) …))
    ((nest-reverse first placeholder (first-step …) (extra-step …) …)
      (let ()
        (define-syntax %nest
          (syntax-rules (placeholder)
            ((%nest result () placeholder ()) result)
            ((%nest result () placeholder (step . rest))
              (%nest () step result rest))
            ((%nest result () accum steps)
              (syntax-error "nest-reverse: step must contain _"))
            ((%nest result (placeholder . rest) placeholder steps)
              (syntax-error "nest-reverse: only one _ allowed per step"))
            ((%nest (result ***) (placeholder . rest) accum steps)
              (%nest (result *** accum) rest placeholder steps))
            ((%nest (result ***) (element . rest) accum steps)
              (%nest (result *** element) rest accum steps))))
        (%nest () (first-step …) first ((extra-step …) …))))
    ((nest-reverse first placeholder) first)))


 


