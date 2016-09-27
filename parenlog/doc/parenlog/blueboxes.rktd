334
((3) 0 () 1 ((q lib "parenlog/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q query-model)) q (311 . 8)) ((c form c (c (? . 0) q :-)) q (301 . 2)) ((c form c (c (? . 0) q define-model)) q (0 . 13)) ((c def c (c (? . 0) q model?)) q (482 . 3)) ((c form c (c (? . 0) q ?)) q (534 . 2)) ((c form c (c (? . 0) q next)) q (543 . 2))))
syntax
(define-model id stmt ...)
 
      stmt = head-query
           | (:- head-query body-query ...)
              
head-query = s-expr
              
body-query = s-expr
           | (,fun s-expr ...)
 
  id : identifier?
  fun : (any/c ... -> boolean?)
syntax
:-
syntax
(query-model model-expr maybe-limit body-query)
 
maybe-limit = 
            | #:limit limit-expr
 
  model-expr : model?
  limit-expr : number?
procedure
(model? v) -> boolean?
  v : any/c
syntax
?
syntax
next
