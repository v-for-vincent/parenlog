#lang racket/base
(require (for-syntax syntax/parse
                     racket/base
                     racket/list
                     racket/function
                     "stx.rkt")
         racket/local
         racket/match
         racket/function
         racket/list
         "stx.rkt"
         racket/generator)

(define-struct model (rules))
(define-struct query ())
(define-struct (sexpr-query query) (se))
(define-struct (fun-query query) (f args))

; env seems to contain bindings for variables
(define (unbound-variable? env q1)
  (and (variable? q1)
       (not (hash-has-key? env q1))))

(define (bound-variable? env q1)
  (and (variable? q1)
       (hash-has-key? env q1)))

(define (unify env q1 q2)
  (cond
    [(equal? q1 q2)
     env]
    [(unbound-variable? env q1)
     ; unification is functional update of the bindings so far
     ; does not return a substitution like my version, but does add the same info to the data so far
     (hash-set env q1 q2)]
    [(unbound-variable? env q2)
     (hash-set env q2 q1)]
    [(bound-variable? env q1)
     (unify env (hash-ref env q1) q2)]
    [(bound-variable? env q2)
     (unify env q1 (hash-ref env q2))]
    [(and (pair? q1) (pair? q2))
     (let ([new-env (unify env (car q1) (car q2))])
       (and new-env
            (unify new-env (cdr q1) (cdr q2))))]
    [else #f]))

; this is just a guard value, generated when Python would throw an exception
(define generator-done
  (local [(define-struct uniq ())]
    (make-uniq)))

; rewrites S-expression syntax
; quotes identifiers other than variables
; so I think the idea is that it no longer contains *Racket* variables (unless they look like Prolog variables)
(define-for-syntax (rewrite-se stx)
  (cond
    [(identifier? stx)
     (if (variable? (syntax->datum stx))
         stx
         (quasisyntax/loc stx (quote #,stx)))]
    [(syntax->list stx)
     ; the following lambda will be applied to the result of syntax->list because of =>
     => (lambda (stxs)
          (quasisyntax/loc stx
            (list #,@(map rewrite-se stxs))))]
    [else
     stx]))

; the body can contain Racket expressions, introduced using the unquote operator
; this is more or less like applying rewrite-se, but taking into account unquote and turning the body into a query
; looks like there are two types: fun-query (for Racket calls) and sexpr-query (for regular Prolog queries)
(define-for-syntax (rewrite-body-query stx)
  (syntax-case stx (unquote) ; unquote is a literal which occurs in the syntax
    [((unquote f) arg ...)
     (quasisyntax/loc stx
       (make-fun-query f #,(rewrite-se #'(arg ...))))]
    [_
     (quasisyntax/loc stx
       (make-sexpr-query #,(rewrite-se stx)))]))

; puts (Prolog) variable syntaxes in a list of single-element lists
; used to rename clauses?
; variables get wrapped in lists due to how compile-rule works
; uses with-syntax [(var ...)]
(define-for-syntax (extract-se stx)
  (cond
    [(identifier? stx)
     (if (variable? (syntax->datum stx))
         (list stx)
         empty)]
    [(syntax->list stx)
     => (curry map extract-se)]
    [else
     empty]))

; see above and below
(define-for-syntax (extract-body stx)
  (syntax-case stx (unquote)
    [((unquote f) arg ...)
     (extract-se #'(arg ...))]
    [_
     (extract-se stx)]))

; gets the S-expression syntaxes for variables
; different for body and head because body may also contain Racket predicates
(define-for-syntax (extract-vars head-stx body-stxs)
  (remove-duplicates
   (flatten
    (cons (extract-se head-stx)
          (map extract-body body-stxs)))
   #:key syntax-e))

; rules are compiled to functions that take a model, an environment and a query
; the downside is that compiled rules cannot be abstracted anymore
; also, this is a syntax transformation
; is it possible to either retain the S-expressions, or to compile to a concrete rule structure?
; the latter would be transformed into a generator function at runtime
(define-syntax (compile-rule stx)
  (syntax-case stx ()
    [(_ head body-query ...)
     (with-syntax ([(var ...) (extract-vars #'head (syntax->list #'(body-query ...)))]
                   [head-sans-vars (rewrite-se #'head)]
                   [(body-query-sans-vars ...) 
                    (map rewrite-body-query
                         (syntax->list #'(body-query ...)))])
       (syntax/loc stx
         (lambda (model env query)
           (define var (gensym 'var))
           ...
           ; this is fine even if query contains multiple conjuncts
           ; it'll only unify matching pairs
           (define new-env (unify env head-sans-vars query))         
           (generator
            () ; these are the formal parameters - there are none
            (when new-env
              (let ([body-sans-vars (list body-query-sans-vars ...)])
                ; this whole thing yields environments (instead of just answer substitutions)
                (reyield yield (model-env-generator/queries model new-env body-sans-vars))))
            (yield generator-done)))))]))

(define (rule-env-generator m r env q)
  (r m env q))

; note: reyield is *syntax*, yield is a *procedure*
; so this is just syntax to say that the generator g should be exhausted
(define-syntax-rule (reyield yield g)
  (for ([ans (in-producer g generator-done)])
    (yield ans)))

; a generator function which generates environments from a model and a conjunction
(define (model-env-generator/queries m env qs)
  (generator
   ()
   (match qs
     [(list) (yield env)]
     [(list-rest q1 qs)
      ; gets the answers for the first conjunct, combines them with the recursively obtained answers for the second, third,... conjuncts
      (for ([new-env (in-producer (model-env-generator m env q1) generator-done)])
        (reyield yield (model-env-generator/queries m new-env qs)))])
   (yield generator-done)))

; generates the answers to a single query
(define (model-env-generator m env first-query)
  (generator
   ()
   ; query can be a regular S-expression (just a Prolog goal)
   ; or it can be a Racket predicate
   (match first-query
     [(struct sexpr-query (q-se))
      (for ([rule (in-list (model-rules m))])
        (reyield yield (rule-env-generator m rule env q-se)))]
     [(struct fun-query (f args))
      (when (apply f (map (curry env-deref env) args))
        (yield env))])
   (yield generator-done)))

; recursively finds the value bound to variable v
(define (env-deref env v)
  (cond
    [(bound-variable? env v)
     (env-deref env (hash-ref env v))]
    [(list? v)
     (map (curry env-deref env) v)]
    [else
     v]))

; restricts the environment to the variables in the list l
(define (env-restrict env l)
  (for/hasheq ([(k v) (in-hash env)]
               #:when (member k l))
    (values k (env-deref env v))))

; makes a list of all the variables in a query
(define (variables-in q)
  (match q
    [(? variable? v) (list v)]
    [(struct fun-query (_ l))
     (append-map variables-in l)]
    [(struct sexpr-query (l))
     (append-map variables-in l)]
    [(? symbol?) empty]
    [(? list? l)
     (append-map variables-in l)]))

; generates answers to one query
(define (query-answer-generator m q)
  (define init-vars (variables-in q))
  (generator
   ()
   (for ([ans (in-producer (model-env-generator m (make-immutable-hasheq empty) q)
                           generator-done)])
     (yield (env-restrict ans init-vars)))
   (yield generator-done)))

; this is like zipping the range and the answer set together
; producer can be a generator, which gives a form of lazy evaluation
; this expects the queries to already be compiled (query-model in expander does not)
; the assumption seems to be that q is atomic
; both the code and the documentation make it clear that q should be a single s-expression
(define (query-model* m q #:limit [limit +inf.0])
  (for/list ([ans (in-producer (query-answer-generator m q) generator-done)]
             [i (in-range limit)])
    ans))

(define-syntax (compile-query stx)
  (syntax-parse
   stx #:literals (unquote)
   [(_ ((unquote f) arg ...))
    (syntax/loc stx
                (make-fun-query f (list 'arg ...)))]
   [(_ query)
    (syntax/loc stx
      (make-sexpr-query 'query))]))

(provide (all-defined-out))
