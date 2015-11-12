;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Amish) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Global Variables
(define listOfOperators '(+ - * /))

;generic helper functions
;returns true if val is found in lst
(define contains
  (lambda (val lst)
    (cond
      ((null? lst) #f)
      ((eq? (car lst) val) #t)
      (else (contains val (cdr lst))))))

;Functions related to the variable environment
(define empty-env
  (lambda () '()))

(define empty-scope
  (lambda () '()))

(define extend-scope
  (lambda (var val scope)
    (cons (list var val) scope)))

(define extend-env
  (lambda (scope env)
    (cons scope env)))

(define apply-scope
  (lambda (var scope)
    (cond
      ((null? scope) #f)
      ((eq? (caar scope) var) (cadar scope))
      (else (apply-scope var (cdr scope))))))

(define apply-env
  (lambda (var env)
    (cond
      ((null? env) #f)
      (else (let ((resolved (apply-scope var (car env))))
        (if (eq? resolved #f)
            (apply-env var (cdr env))
            resolved))))))

(define extend-env-4-lambda-helper
  (lambda (lovars lovals scope)
    (cond
      ((not (null? lovars)) (extend-env-4-lambda-helper
                             (cdr lovars)
                             (cdr lovals)
                             (extend-scope (car lovars) (car lovals) scope)))
      (else scope))))

(define extend-env-4-lambda
  (lambda (lovars lovals env)
    (extend-env
     (extend-env-4-lambda-helper lovars lovals (empty-scope))
     env)))


;Constructors related to the LCE types
(define lit-exp
  (lambda (lit) lit))

(define var-exp
  (lambda (id) id))

(define lambda-exp
  (lambda (params body)
    (list 'lambda params body)))

(define app-exp
  (lambda (rator rands)
    (append (list rator) rands)))


;return true if the given symbol is a reserved op and false otherwise

;Parser Helper Functions
;returns true is s is an operator and false otherwise
(define op?
  (lambda (s)
    (contains s listOfOperators)))

;Core Parser Functions
(define parse-exp
  (lambda (lcExp)
    (cond
      ((number? lcExp) (list 'lit-exp lcExp))
      ((symbol? lcExp) (if (op? lcExp)
                           (list 'op-exp lcExp)
                           (list 'var-exp lcExp)))
      ((eq? (car lcExp) 'lambda)
       (list 'lambda-exp
             (cadr lcExp)
             (parse-exp (caddr lcExp))))
      (else (cons 'app-exp (append (list (parse-exp (car lcExp))) (map parse-exp (cdr lcExp))))))))

;evaluates an app expression whose car is an operator
(define eval-op-exp
  (lambda (appExp env)
    appExp))
    
(define eval-exp
  (lambda (lce env)
    (cond
      ((eq? (car lce) 'lit-exp) (cadr lce))
      ((eq? (car lce) 'var-exp) (apply-env (cadr lce) env))
      ((eq? (car lce) 'lambda-exp) (eval-exp (caddr lce) env))
      (else
       (cond
         ((eq? (list-ref (list-ref lce 1) 0) 'lambda-exp)
           ;first element of app-exp is a lambda
           (eval-exp (list-ref (list-ref lce 1) 2)
                     (extend-env-4-lambda
                      (list-ref (list-ref lce 1) 1)
                      (map (lambda (x)
                             (if (eq? (car x) 'lambda-exp)
                                 x
                                 (eval-exp x env))) (cddr lce)) env)))
         ((eq? (list-ref (list-ref lce 1) 0) 'op-exp)
          ;first element of app-exp is a op-exp
         (else
          ;first element of app-exp is a var-exp
           (let ((theLambda (eval-exp (list-ref lce 1) env))
                 (theInputs (map (lambda (x)
                             (if (eq? (car x) 'lambda-exp)
                                 x
                                 (eval-exp x env))) (cddr lce))))
             (eval-exp theLambda (extend-env-4-lambda (list-ref theLambda 1)
                                                      theInputs
                                                      env)))))))))
           


(define run-program
  (lambda (lce)
    (eval-exp lce (empty-env))))


(define anExp '((lambda (a b c) (a b c)) (lambda (x y) (+ x y)) 5 6))
;(define anExp '(lambda (a b) (a b)))

;Pass the above to apply-env to make sure it comes out
(parse-exp anExp)
;(run-program (parse-exp anExp))
