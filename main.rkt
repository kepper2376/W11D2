#lang racket
(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

; Week 11 Day 2: tagged values, sections 1-8.
; The void wrappers suppress DrRacket printing the tagged return
; value from each top-level test; fire still prints the plain value.
; Starting scope: a = 1, b = 2, c = "ccc", e = True, f = False

; 1. variables and literals
(void (process (parse '(
  main
  (fire a)        ;1
  (fire "hello")  ;hello
  (fire True)     ;True
))))

; 2. math, and using a result again
(void (process (parse '(
  main
  (fire (a + b))          ;3
  (fire ((a + b) * 2))    ;6
  (n = ((a + b) * 2))
  (fire (n - 1))          ;5
))))

; 3. comparison and logic
(void (process (parse '(
  main
  (fire (a < b))          ;True
  (g = (a < b))
  (fire (g && e))         ;True
  (fire (! e))            ;False
))))

; 4. strings and equality
(void (process (parse '(
  main
  (s = "abc")
  (fire s)                ;abc
  (fire (s == "abc"))     ;True
  (fire (a === c))        ;False
))))

; 5. conditions must be a boolean, and False must really be false
(void (process (parse '(
  main
  (ask f ((fire "wrong")) ((fire "right")))   ;right
  (running = True)
  (k = 0)
  (while running (
    (k = (k + 1))
    (ask (k == 3) ((running = False)) ())
  ))
  (fire k)                ;3
  (more = False)
  (m = 0)
  (do-while ((m = (m + 1))) more)
  (fire m)                ;1
))))

; 6. functions
(void (process (parse '(
  main
  (func add (x y) (
    (z = (x + y))
    (return z)
  ))
  (fire (call add (3 4)))       ;7
  (func is_big (x) (
    (ask (x > 10) ((return True)) ((return False)))
  ))
  (fire (call is_big (20)))     ;True
  (func nothing () (
    (z = 1)
  ))
  (fire (call nothing ()))      ;void
))))

 ; 7. gcd: Euclid's algorithm using a loop and recursion.
(void (process (parse '(
  main
  (func gcd_loop (x y) (
    (while (! (y == 0)) (
      (temp = y)
      (y = (x % y))
      (x = temp)
    ))
    (return x)
  ))
  (func gcd_rec (x y) (
    (ask (y == 0)
         ((return x))
         ((return (call gcd_rec (y (x % y))))))
  ))
))))

(void (process (parse '(fire (call gcd_loop (48 18))))))   ;6
(void (process (parse '(fire (call gcd_loop (17 5))))))    ;1
(void (process (parse '(fire (call gcd_loop (100 75))))))  ;25
(void (process (parse '(fire (call gcd_rec (48 18))))))    ;6
(void (process (parse '(fire (call gcd_rec (17 5))))))     ;1
(void (process (parse '(fire (call gcd_rec (100 75))))))   ;25

; 8. All stored values, including functions, are tagged.
(displayln environment)
