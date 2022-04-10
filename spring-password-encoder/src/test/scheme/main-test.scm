(define-library
  (main-test)

  (import (scheme base)
          (srfi 64)
          (arvyy spring-password-encoder))

  (export run-tests)

  (begin
    (define (run-tests)

      (define (test-encoder encoder)
        (define password "Foo")
        (define encoded (encode-password encoder password))
        (test-assert (passwords-match encoder "Foo" encoded))
        (test-assert (not (passwords-match encoder "Bar" encoded))))

      (test-begin "Spring password encoder")

      (test-group
        "BCrypt encoder"
        (test-encoder (make-bcrypt-password-encoder))
        (test-encoder (make-bcrypt-password-encoder 8)))

      (test-group
        "SCrypt encoder"
        (test-encoder (make-scrypt-password-encoder))
        (test-encoder (make-scrypt-password-encoder 4 8 1 32 64)))

      (test-group
        "Argon2 encoder"
        (test-encoder (make-argon2-password-encoder))
        (test-encoder (make-argon2-password-encoder 10 10 2 4 2)))

      (test-group
        "Pbkdf2 encoder"
        (test-encoder (make-pbkdf2-password-encoder))
        (test-encoder (make-pbkdf2-password-encoder "secret"))
        (test-encoder (make-pbkdf2-password-encoder "secret" 2))
        (test-encoder (make-pbkdf2-password-encoder "secret" 2 16))
        (test-encoder (make-pbkdf2-password-encoder "secret" 2 2 16)))

      (test-group
        "Custom encoder"
        (test-encoder (make-custom-password-encoder
                        (lambda (password)
                          password)
                        (lambda (raw encrypted)
                          (string=? raw encrypted)))))

      (test-end))))
