(define-library
    (arvyy solrj)
    (export
            create-http-solr-client

            commit-within
            commit
            rollback

            add
            query
            delete-by-query
            delete-by-id
            )
    (import (scheme base)
            (scheme write)
            (srfi 1)
            (class java.util ArrayList)
            (class java.util List)
            (class java.util Map)
            (class org.apache.solr.client.solrj.response Suggestion)
            (class org.apache.solr.client.solrj.response QueryResponse)
            (class org.apache.solr.client.solrj.response SuggesterResponse)
            (class org.apache.solr.client.solrj SolrClient)
            (class org.apache.solr.client.solrj.impl HttpSolrClient)
            (class org.apache.solr.common SolrDocumentList)
            (class org.apache.solr.common SolrInputDocument)
            (class org.apache.solr.common SolrDocument)
            (class org.apache.solr.common.params SolrParams)
            (class org.apache.solr.common.params ModifiableSolrParams)
            (class org.apache.solr.client.solrj SolrQuery)
            (class org.apache.solr.client.solrj.impl NoOpResponseParser)
            (class org.apache.solr.client.solrj SolrRequest)
            (class org.apache.solr.client.solrj.request UpdateRequest)
            (class org.apache.solr.client.solrj.request QueryRequest)
            (class org.apache.solr.common.util NamedList)
            )
    (include "solrj-impl.scm"))
