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
  (define ctxt #'different?)
  (define wrapped-ses (map wrap-fact ses))
  (make-model (map (λ (wse) (i/rule wse ctxt)) wrapped-ses)))

(provide define-model
         query-model
         :-
         i/model
         i/query
         query-model*
         i/rule
         model?)

(define different? (compose not equal?))
(define my-model (i/model `((parent rogue moria)
                            (parent rogue larn)
                            (parent rogue omega)
                            (parent rogue hack)
                            (parent moria angband)
                            (parent hack nethack)
                            (parent angband tome)
                            (parent angband zangband)
                            (parent omega adom)
                            (parent nethack adom)
                            (parent nethack zapm)
                            (parent nethack slashem)
                            (parent nethack crawl)
                            (:- (sibling X Y)
                                (parent Z X)
                                (parent Z Y)
                                (,different? X Y)))))
(query-model* my-model (i/query '(sibling X Y)))
