#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         (only-in racket/function curry)
         "core.rkt"
         racket/match)

; TODO move comments to proper docs

(define-syntax (:- stx)
  (raise-syntax-error ':- "Cannot be used outside define-model" stx))

(define-syntax (paren-stmt->rule stx)
  (syntax-parse
      stx
    #:literals (:-)
    [(_ (:- head body-query ...))
     (syntax/loc stx
       (compile-rule head
                     body-query ...))]
    [(_ head)
     (syntax/loc stx
       (paren-stmt->rule (:- head)))]))

(define-syntax (define-model stx)
  (syntax-parse
      stx
    [(_ model:id paren-stmt ...)
     (syntax/loc stx
       (define model
         (make-model
          (list (paren-stmt->rule paren-stmt)
                ...))))]))

(define-syntax (query-model stx)
  (syntax-parse 
      stx
    [(_ model:expr options ... query)
     (syntax/loc stx
       (query-model* model options ...
                     (compile-query query)))]))

(define (i/model ses) ; ses is supplied as a *list* of expressions, so we only have to (quasi-)quote one thing
  (define (wrap-fact qse)
    (match qse
      [(list-rest ':- rest) qse]
      [_ (list ':- qse)]))
  (make-model (map (compose i/rule wrap-fact) ses)))

(provide define-model
         query-model
         :-
         i/model
         i/query
         query-model*
         i/rule
         model?)
