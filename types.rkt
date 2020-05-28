#lang racket
;;;;;;;;;;;;;;;
#| EXPANDER |#
;;;;;;;;;;;;;;;
(define (expand-expr expr)
  (match expr
    [`(let ([,id ,val])
        ,body)
     `(,(expand-expr body))]
    [`(let ([,ids ,vals] ...)
        ,body)
      `(let ([,(car ids) ,(car vals)])
         ,(expand-expr `(let ,(map list (cdr ids) (cdr vals))
                          ,body)))]
    [`(λ ([,id ,ty])
         ,body)
      `(λ ([,id ,ty])
          ,(expand-expr body))]
    [`(λ ([,ids ,tys] ...)
         ,body)
      `(λ ([,(car ids) ,(car tys)])
          ,(expand-expr `(λ ,(map list (cdr ids) (cdr tys))
                            ,body)))]
    [`(,f ,x)
      `(,(expand-expr f) ,(expand-expr x))]
    [`(,f ,x . ,xs)
      (expand-expr `((,f ,x) . ,xs))]
    [_ expr]))
;;;;;;;;;;;;;;;;;;
#| TYPE-CHECKER |#
;;;;;;;;;;;;;;;;;;
(require racket/hash)

(struct type () #:transparent)
(struct int type () #:transparent)
(struct bool type () #:transparent)
(struct proc type (arg-type result-type) #:transparent)

(define + (proc (int) (proc (int) (int))))
(define base-tenv (hash '+ +))

(define (external-form->type texpr)
  (match texpr
    ['int (int)]
    ['bool (bool)]
    [`(λ (,ty)
         ,body-ty)
      (proc (external-form->type ty) (external-form->type body-ty))]))

(define (extend-tenv id ty tenv)
  (hash-set tenv id ty))

(define (type->external-form ty)
  (match ty
    [(int) 'int]
    [(bool) 'bool]
    [(proc arg-type result-type)
     (list (type->external-form arg-type)
           '->
           (type->external-form result-type))]))

(define (type-of-procedure id ty body tenv)
  (let* ([arg-type (external-form->type ty)]
         [result-type (type-of body
                              (extend-tenv id arg-type tenv))])
    (proc arg-type result-type)))

(define (type-of-application rator-type rand-type)
  (match rator-type
    [(proc arg-type result-type)
     (if (equal? arg-type rand-type)
       result-type
       (error (format "operand type ~a does not match argument type ~a" (type->external-form arg-type)
                      (type->external-form rand-type))))]
    [(not (? proc?))
     (error (format "cannot apply ~s to ~s" (type->external-form rator-type)
                    (type->external-form rand-type)))]))

(define (types-of exprs tenv)
  (map (curryr type-of tenv) exprs))

(define (type-of expr tenv)
  (match expr
    [(? number?) (int)]
    [(? boolean?) (bool)]
    [(? symbol?) (hash-ref tenv expr)]
    [`(let ([,id  ,val])
        ,body)
      (type-of body (extend-tenv id (type-of val tenv) tenv))]
    [`(λ (( ,id ,ty)) ,body)
      (type-of-procedure id ty body tenv)]
    [`(,f ,x)
      (type-of-application (type-of f tenv)
                           (type-of x tenv))]))

(type->external-form (type-of (expand-expr '((λ ([x int] [y int]) (+ x y)) 4 5)) base-tenv))
