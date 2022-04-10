(define-syntax define-interface
  (syntax-rules ()
    ((_ constructor
        (name args ... . rest) ...)
     (h 0 constructor 0 () ((name args ... . rest) ...)))))

(define-syntax validate-signature
  (syntax-rules ()
    ((_ (name1 rest1 ... . rest1*) (name2 rest2 ... . rest2*))
     (let-syntax ((test (syntax-rules (name1)
                          ((test name1) (validate-args-count name1 (rest1 ... . rest1*) (rest2 ... . rest2*)))
                          ((test _) (syntax-error "Name mismatch in interface implementation (expected, gotten)" name1 name2)))))
       (test name2)))))

(define-syntax validate-args-count
  (syntax-rules ()
    ((_ name (a a* ... . rest1) (b b* ... . rest2))
     (validate-args-count name (a* ... . rest1) (b* ... . rest2)))
    ((_ name () ())
     (begin))
    ((_ name () (b* ...))
     (syntax-error "Arg count mismatch in " name))
    ((_ name (a* ...) ())
     (syntax-error "Arg count mismatch in " name))
    ((_ name (a* ...) args)
     (begin))
    ((_ name args1 args2)
     (begin))
    ((_ . any)
     (syntax-error "Bad syntax"))))

(define-syntax validate-signatures
  (syntax-rules ()
    ((_ (sig1 sig1-rest ...) (sig2 sig2-rest ...))
     (begin
       (validate-signature sig1 sig2)
       (validate-signatures (sig1-rest ...) (sig2-rest ...))))
    ((_ () ())
     (begin))
    ((_ (sig1 ...) ())
     (syntax-error "Implementation doesn't provide all functions"))
    ((_ () (sig2 ...))
     (syntax-error "Implementation provides more functions than in interface"))))

(define-syntax define-external
  (syntax-rules ()
    ((_ name index impl (args ...) ())
     (define (name impl args ...)
       ((vector-ref impl index) args ...) ))
    ((_ name index impl (args ...) rest)
     (define (name impl args ... . rest)
       (apply (vector-ref impl index) args ... rest) ))))

(define-syntax h
  (syntax-rules ()
    ((_ 0 constructor index (parsed-case ...) ((name args ... . args*) rest ...))
     (h 0 constructor (+ 1 index) (parsed-case ... (index name args ... . args*)) (rest ...)))
    ((_ 0 constructor index parsed-cases ())
     (h 1 constructor parsed-cases))
    
    ((_ 1 constructor ((index name args ... . rest) ...))
     (begin
       (define-external name index impl (args ...) rest) ...
       (define-syntax constructor
         (syntax-rules ...* (name ...)
           ((_ ((name* args* ...* . rest*) body ...*) ...*)
            (begin
              (validate-signatures ((name args ... . rest) ...) ((name* args* ...* . rest*) ...*))
              (vector
                (lambda (args* ...* . rest*) body ...*) ...*)))))))))
