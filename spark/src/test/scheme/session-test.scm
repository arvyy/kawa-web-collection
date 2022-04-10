(define-library
  (session-test)
  (import 
    (scheme base)
    (scheme write)
    (srfi 64)
    (srfi 95) ;sort
    (arvyy kawa-spark)
    (test-util)
    (class org.apache.http.client.fluent Request Response Content Form)
    (class org.apache.http.entity ContentType))
  (export do-session-test)
  (begin
    (define (do-session-test)
      #t
      )))
