#lang racket

(define scope
  (list (list 'a (list 'num 1))
        (list 'b (list 'num 2))
        (list 'c (list 'str "ccc"))
        (list 'e (list 'bool #t))
        (list 'f (list 'bool #f))))
(define environment (list scope))

; Every language value is a two-element (type value) list.
(define tagged?
  (lambda (v) (and (list? v) (= (length v) 2) (symbol? (car v)))))
(define check_type
  (lambda (type pair)
    (if (and (tagged? pair) (eq? type (car pair)))
        (cadr pair)
        (error (string-append "Data type error: expect " (symbol->string type) ".")))))
(define void-value (list 'void 'void))

(define resolve
  (lambda (var_scope var_name)
    (cond ((null? var_scope) (void))
          ((eq? (caar var_scope) var_name) (cadar var_scope))
          (else (resolve (cdr var_scope) var_name)))))
(define resolve_env
  (lambda (var_env var_name)
    (cond ((null? var_env) (void))
          ((void? (resolve (car var_env) var_name)) (resolve_env (cdr var_env) var_name))
          (else (resolve (car var_env) var_name)))))

(define update_pair
  (lambda (left_part lst key value)
    (cond ((null? lst) left_part)
          ((eq? (caar lst) key) (append left_part (list (list key value)) (cdr lst)))
          (else (update_pair (append left_part (list (car lst))) (cdr lst) key value)))))
(define update_scope
  (lambda (var_scope var_name var_value)
    (unless (tagged? var_value) (error "Scope requires tagged values" var_value))
    (if (void? (resolve var_scope var_name))
        (cons (list var_name var_value) var_scope)
        (update_pair '() var_scope var_name var_value))))
(define wolf_env
  (lambda (var_env var_name var_value)
    (define (update-environment scopes)
      (cond ((null? scopes) '())
            ((void? (resolve (car scopes) var_name))
             (cons (car scopes) (update-environment (cdr scopes))))
            (else (cons (update_scope (car scopes) var_name var_value) (cdr scopes)))))
    (unless (tagged? var_value) (error "Scope requires tagged values" var_value))
    (if (void? (resolve_env var_env var_name))
        (set! environment (cons (update_scope (car var_env) var_name var_value) (cdr var_env)))
        (set! environment (update-environment var_env)))
    var_value))

(define is_valid_math_op
  (lambda (op) (and (memq op '(+ - * / // %)) #t)))
(define is_valid_boolean_op
  (lambda (op) (and (memq op '(> >= < <= == === && ||)) #t)))

(define do_math
  (lambda (op left_operand right_operand)
    (let ((left_val (check_type 'num left_operand))
          (right_val (check_type 'num right_operand)))
      (list 'num
            (cond ((eq? op '+) (+ left_val right_val))
                  ((eq? op '-) (- left_val right_val))
                  ((eq? op '*) (* left_val right_val))
                  ((eq? op '/) (/ left_val right_val))
                  ((eq? op '//) (quotient left_val right_val))
                  ((eq? op '%) (modulo left_val right_val))
                  (else (error "Unknown math operator" op)))))))

(define calculate_boolean
  (lambda (op left_operand right_operand)
    (list 'bool
          (cond
            ((eq? op '>) (> (check_type 'num left_operand) (check_type 'num right_operand)))
            ((eq? op '>=) (>= (check_type 'num left_operand) (check_type 'num right_operand)))
            ((eq? op '<) (< (check_type 'num left_operand) (check_type 'num right_operand)))
            ((eq? op '<=) (<= (check_type 'num left_operand) (check_type 'num right_operand)))
            ; == checks matching types, then compares their payloads.
            ((eq? op '==) (and (eq? (car left_operand) (car right_operand))
                               (equal? (cadr left_operand) (cadr right_operand))))
            ; === compares the complete tagged values.
            ((eq? op '===) (equal? left_operand right_operand))
            ((eq? op '&&) (and (check_type 'bool left_operand) (check_type 'bool right_operand)))
            ((eq? op '||) (or (check_type 'bool left_operand) (check_type 'bool right_operand)))
            (else (error "Unknown boolean operator" op))))))

(define create_pair_list
  (lambda (lst_ret lst1 lst2)
    (if (or (null? lst1) (null? lst2))
        (reverse lst_ret)
        (create_pair_list (cons (list (car lst1) (car lst2)) lst_ret) (cdr lst1) (cdr lst2)))))
(define update_base_environment (lambda (new_env) (set! environment new_env)))
(define push_scope_to_environment (lambda (new_scope) (set! environment (cons new_scope environment))))
(define pop_scope_from_environment (lambda () (set! environment (cdr environment))))

(provide (all-defined-out))
