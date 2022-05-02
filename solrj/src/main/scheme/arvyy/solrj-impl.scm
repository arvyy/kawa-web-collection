(define (create-http-solr-client url ::String) ::SolrClient
    (let* ((builder (HttpSolrClient:Builder))
           (builder (builder:withBaseSolrUrl url)))
      (builder:build)))

(define commit-within (make-parameter #f))

(define (scm-value->solr-value scm-value)
    (cond
        ((string? scm-value) scm-value)
        ((boolean? scm-value) (->boolean scm-value))
        ((integer? scm-value) (->int scm-value))
        ((real? scm-value) (->double scm-value))
        ((vector? scm-value)
         (let ((lst (ArrayList)))
           (for-each
             (lambda (index)
               (lst:add (scm-value->solr-value (vector-ref scm-value index))))
             (iota (vector-length scm-value)))
           lst))))

(define (list->solr-input-doc alist) ::SolrInputDocument
    (define sid (SolrInputDocument))
    (for-each
        (lambda (e)
            (sid:setField (symbol->string (car e)) (scm-value->solr-value (cdr e))))
        alist)
    sid)

(define (parse-response-value value)
    (define (parse-named-list lst ::NamedList)
        (map
            (lambda (index)
                (cons
                    (string->symbol (lst:getName index))
                    (parse-response-value (lst:getVal index))))
            (iota (lst:size))))
    (define (solr-document->json doc ::SolrDocument)
        (map
            (lambda (key)
                (cons (string->symbol key)
                      (parse-response-value (doc:get key))))
            (doc:keySet)))
    (define (parse-plain-list lst ::List)
        (define v (make-vector (lst:size)))
        (for-each
            (lambda (index)
                (vector-set! v index (parse-response-value (lst:get index))))
            (iota (vector-length v)))
        v)
    (define (parse-document-list lst ::SolrDocumentList)
        `((max-score . ,(lst:getMaxScore))
          (num-found . ,(lst:getNumFound))
          (start . ,(lst:getStart))
          (docs . ,(parse-plain-list lst))))
    (define (parse-map m ::Map)
        (map
            (lambda (key)
                `(,(string->symbol key) . ,(parse-response-value (m:get key))))
            (m:keySet)))
    (cond
        ((or (string? value) (number? value) (boolean? value)) value)
        ((NamedList? value) (parse-named-list value))
        ((SolrDocumentList? value) (parse-document-list value))
        ((List? value) (parse-plain-list value))
        ((SolrDocument? value) (solr-document->json value))
        ((Map? value) (parse-map value))))

#|
(define (parse-suggester-response resp ::SuggesterResponse)
    (define (suggestion->alist s ::Suggestion)
        `((payload . ,(s:getPayload))
          (term . ,(s:getTerm))
          (weight . ,(s:getWeight))))
    (define terms (resp:getSuggestions))
    (map
        (lambda (key)
            `(,key . ,(list->vector (map suggestion->alist (terms:get key)))))
        (terms:keySet)))
|#

(define (list->solr-params handler alist) ::SolrParams
  (define (toString value)
    (cond
      ((boolean? value) (if value "true" "false"))
      (else (value:toString))))
  (define p (SolrQuery))
  (for-each
    (lambda (e)
        (cond
            ((vector? (cdr e))
             (for-each
                (lambda (entry)
                    (p:add (symbol->string (car e)) (toString entry)))
                (cdr e)))
             (else (p:add (symbol->string (car e)) (toString (cdr e))))))
    alist)
  (p:setRequestHandler handler)
  p)


(define (execute-request client ::SolrClient core ::String request ::SolrRequest)
    (let* ((solr-resp (request:process client core))
           (resp (solr-resp:getResponse))
           (parsed-response (parse-response-value resp)))
        parsed-response))

(define (query solr-client ::SolrClient core ::String handler ::String params-alist)
    (define request (QueryRequest (list->solr-params handler params-alist)))
    (execute-request solr-client core request))

(define (add solr-client ::SolrClient core ::String docs)
    (define request (UpdateRequest))
    (define commit (commit-within))
    (when commit
        (request:setCommitWithin commit))
    (vector-for-each
        (lambda (doc)
            (request:add (list->solr-input-doc doc)))
        docs)
    (execute-request solr-client core request))

(define (delete-by-query solr-client ::SolrClient core ::String query ::String)
    (define request (UpdateRequest))
    (define commit (commit-within))
    (when commit
        (request:setCommitWithin commit))
    (request:deleteByQuery query)
    (execute-request solr-client core request))

(define (delete-by-id solr-client ::SolrClient core ::String ids)
    (define request (UpdateRequest))
    (define commit (commit-within))
    (when commit
        (request:setCommitWithin commit))
    (vector-for-each
        (lambda (id ::String)
            (request:deleteById id))
        ids)
    (execute-request solr-client core request))

(define (commit solr-client ::SolrClient core ::String)
    (solr-client:commit core))

(define (rollback solr-client ::SolrClient core ::String)
    (solr-client:rollback core))