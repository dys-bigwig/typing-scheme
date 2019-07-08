#lang racket
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
    ['bool (bool)]))

(define (extend-tenv id ty tenv)
  (hash-set tenv id (external-form->type ty)))

(define (type->external-form ty)
  (match ty
    [(int) 'int]
    [(bool) 'bool]
    [(proc arg-type result-type)
     (list (type->external-form arg-type)
           '->
           (type->external-form result-type))]))

(define (type-of-procedure id ty body tenv)
  (let ([arg-type (external-form->type ty)]
        [result-type (type-of body
                              (extend-tenv id ty tenv))])
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

(define (type-of expr tenv)
  (match expr
    [(? number?) (int)]
    [(? boolean?) (bool)]
    [(? symbol?) (hash-ref tenv expr)]
    [`(λ (,id : ,ty) ,body)
      (type-of-procedure id ty body tenv)]
    [`(,f ,x)
      (type-of-application (type-of f tenv)
                           (type-of x tenv))]))

(type-of '(λ (x : int) (λ (y : int) ((+ x) y))) base-tenv)
