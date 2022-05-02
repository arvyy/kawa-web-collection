(define-library
    (main-test)
    (import (arvyy solr-embedded)
            (arvyy solrj)
            (scheme base)
            (scheme write)
            (srfi 64))
    (export run-tests)
    (begin
        (define (json-descend json . path)
            (cond
                ((null? path) json)
                (else (let ((p (car path)))
                        (cond
                            ((number? p) (apply json-descend (vector-ref json p) (cdr path)))
                            ((symbol? p) (apply json-descend (cdr (assoc p json)) (cdr path)))
                            (else (error "Bad path")))))))
        (define (test-equal/field expected alist field)
            (test-equal expected (cdr (assoc field alist))))
        (define (run-tests)
            (define client (create-embedded-solr-client "src/test/resources/solrhome" "core1"))
            (delete-by-query client "core1" "*:*")
            (add client "core1" #(((type_s . "bar"))))
            (add client "core1" #(((type_s . "bar"))
                                  ((string_s . "test3")
                                   (boolean_b . #t)
                                   (int_i . 2)
                                   (double_d . 2.5)
                                   (strings_ss . #("foo" "bar"))
                                   (type_s . "baz"))))
            (commit client "core1")
            ;;TODO for some reason commit doesn't happen immediately
            ;;check if there is a way to remove sleep hack
            (java.lang.Thread:sleep 5000)

            ;; test base mapping
            (let* ((qr (query client "core1" "select" '((q . "type_s: baz"))))
                   (docs (json-descend qr 'response 'docs)))
              (test-equal 1 (vector-length docs))
              (let ((doc (vector-ref docs 0)))
                (test-equal/field "test3" doc 'string_s)
                (test-equal/field #t doc 'boolean_b)
                (test-equal/field 2 doc 'int_i)
                (test-approximate 2.5 (cdr (assoc 'double_d doc)) 0.001)
                (test-equal/field #("foo" "bar") doc 'strings_ss)))

            ;; test facets
            (let* ((qr (query client "core1" "select" '((facet . #t) (facet.field . "type_s") (q . "*:*"))))
                   (facets (json-descend qr 'facet_counts)))
              (test-equal 2 (json-descend qr 'facet_counts 'facet_fields 'type_s 'bar))
              (test-equal 1 (json-descend qr 'facet_counts 'facet_fields 'type_s 'baz)))
            )))