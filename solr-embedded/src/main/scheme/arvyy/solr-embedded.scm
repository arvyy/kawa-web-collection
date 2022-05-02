(define-library
    (arvyy solr-embedded)
    (export create-embedded-solr-client)
    (import (scheme base)
            (class org.apache.solr.client.solrj.embedded EmbeddedSolrServer)
            (class org.apache.solr.client.solrj SolrClient)
            (class java.nio.file Path))
    (begin
        (define (create-embedded-solr-client solrhome ::String default-core ::String) ::SolrClient
            (EmbeddedSolrServer (Path:of solrhome) default-core))))