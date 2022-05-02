(define-library
    (main-test)
    (import (arvyy solr-embedded)
            (scheme base)
            (scheme write)
            (srfi 64))
    (export run-tests)
    (begin
        (define (run-tests)
            (test-assert #t))))
