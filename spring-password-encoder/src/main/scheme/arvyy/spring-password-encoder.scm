(define-library
    (arvyy spring-password-encoder)
  (import (kawa base)
          (scheme case-lambda)
          (class org.springframework.security.crypto.password
            PasswordEncoder
            Pbkdf2PasswordEncoder)
          (class org.springframework.security.crypto.scrypt
            SCryptPasswordEncoder)
          (class org.springframework.security.crypto.bcrypt
            BCryptPasswordEncoder)
          (class org.springframework.security.crypto.argon2
            Argon2PasswordEncoder))

  (export make-custom-password-encoder
          make-pbkdf2-password-encoder
          make-bcrypt-password-encoder
          make-scrypt-password-encoder
          make-argon2-password-encoder

          encode-password
          passwords-match)

  (begin

    (define-simple-class CustomPwEncoder (PasswordEncoder)
      (encode-proc)
      (matches-proc)
      ((*init* encode-proc* matches-proc*)
       (set! encode-proc encode-proc*)
       (set! matches-proc matches-proc*))
      ((encode raw)
       (encode-proc raw))
      ((matches raw encoded)
       (matches-proc raw encoded))
      ((upgradeEncoding encoded)
       #f))

    (define (make-custom-password-encoder encode-proc matches-proc)
      (CustomPwEncoder encode-proc matches-proc))

    (define make-pbkdf2-password-encoder
      (case-lambda
        (() (Pbkdf2PasswordEncoder))
        ((secret)
         (Pbkdf2PasswordEncoder secret))
        ((secret salt-length)
         (Pbkdf2PasswordEncoder secret salt-length))
        ((secret iterations hash-width)
         (Pbkdf2PasswordEncoder secret iterations hash-width))
        ((secret salt-length iterations hash-width)
         (Pbkdf2PasswordEncoder secret salt-length iterations hash-width))))

    (define make-bcrypt-password-encoder
      (case-lambda
        (() (BCryptPasswordEncoder))
        ((strength ::int) (BCryptPasswordEncoder strength))))

    (define make-scrypt-password-encoder
      (case-lambda
        (() (SCryptPasswordEncoder))
        ((cpu-cost mem-cost parallelization key-length salt-length)
         (SCryptPasswordEncoder cpu-cost mem-cost parallelization key-length salt-length))))

    (define make-argon2-password-encoder
      (case-lambda
        (() (Argon2PasswordEncoder))
        ((salt-length hash-length parallelization memory iterations)
         (Argon2PasswordEncoder salt-length hash-length parallelization memory iterations))))

    (define (encode-password encoder ::PasswordEncoder password ::String)
      (encoder:encode password))

    (define (passwords-match encoder ::PasswordEncoder rawPassword ::String encryptedPassword ::String)
      (encoder:matches rawPassword encryptedPassword))))
