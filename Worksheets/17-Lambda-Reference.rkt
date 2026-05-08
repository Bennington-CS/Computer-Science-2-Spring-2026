#lang htdp/isl+
;;;;;
;;;;; Lambda Worksheet — ANSWER KEY
;;;;; ISL+ (Intermediate Student with Lambda)
;;;;;

#|
A lambda expression creates an anonymous function -- a function with no
name.  The syntax is:

  (lambda (param ...) body)

This is equivalent to writing (define (f param ...) body) and then
referring to f by name -- except that no name is introduced.  The main
use of lambda is as an argument to an abstraction like map, filter,
foldr, build-list, andmap, ormap, and sort.  Instead of defining a
helper separately, you write it inline.
|#

;;;;
;;;; Part 1 — Reading Lambda Expressions
;;;;

;;; Q1.
(check-expect ((lambda (x) (* x x)) 5)
              25)

;;; Q2.
(check-expect ((lambda (s) (string-length s)) "hello")
              5)

;;; Q3.
(check-expect ((lambda (x y) (+ x y)) 3 4)
              7)

;;; Q4.
(check-expect (map (lambda (x) (+ x 1)) '(10 20 30))
              '(11 21 31))

;;; Q5.
(check-expect (filter (lambda (n) (> n 3)) '(1 2 3 4 5))
              '(4 5))

;;; Q6.
(check-expect (map (lambda (s) (string-append s "!")) '("hi" "bye"))
              '("hi!" "bye!"))

;;; Q7.
(check-expect (build-list 5 (lambda (i) (* i i)))
              '(0 1 4 9 16))

;;; Q8.
(check-expect (foldr (lambda (n acc) (+ n acc)) 0 '(1 2 3 4))
              10)

;;; Q9.
(check-expect (andmap (lambda (n) (even? n)) '(2 4 6))
              #true)

;;; Q10.
(check-expect (ormap (lambda (n) (even? n)) '(1 3 4 7))
              #true)

;;;;
;;;; Part 2 — Translating to Lambda
;;;;

;;; Q11.
(check-expect (map (lambda (n) (* 2 n)) '(1 2 3 4 5))
              '(2 4 6 8 10))

;;; Q12.
(check-expect (filter (lambda (n) (< n 0)) '(3 -1 0 -4 2))
              '(-1 -4))

;;; Q13.
(check-expect (build-list 6 (lambda (i) (* i i)))
              '(0 1 4 9 16 25))

;;; Q14.
(check-expect (sort '("hi" "elephant" "cat")
                    (lambda (s t) (> (string-length s) (string-length t))))
              '("elephant" "cat" "hi"))

;;; Q15.
(check-expect (map (lambda (s) (string-append s "!")) '("wow" "yes" "ok"))
              '("wow!" "yes!" "ok!"))

;;;;
;;;; Part 3 — Writing Lambda from Scratch
;;;;

;;; Q16.  Use map.
(check-expect (map (lambda (s) (string-length s)) '("apple" "fig" "mango"))
              '(5 3 5))

;;; Q17.  Use filter.
(check-expect (filter (lambda (s) (> (string-length s) 3)) '("hi" "hello" "ok" "world"))
              '("hello" "world"))

;;; Q18.  Use build-list.
(check-expect (build-list 5 (lambda (i) (* i 3)))
              '(0 3 6 9 12))

;;; Q19.  Use foldr.
(check-expect (foldr (lambda (n acc) (if (> n 10) (add1 acc) acc)) 0 '(5 12 3 20 8 11))
              3)

;;; Q20.  Use andmap.
(check-expect (andmap (lambda (s) (string=? (substring s 0 1) "a")) '("apple" "avocado" "apricot"))
              #true)

;;;;
;;;; Part 4 — Lambda Over a Parameter (Closures)
;;;;

;;; Q21.
(define THRESHOLD 10)

(check-expect (filter (lambda (n) (< n THRESHOLD)) '(5 10 15 3 20))
              '(5 3))

;;; Q22.
;;; The blank is: factor
;;;
;;; Version B -- using lambda
#;
(define (scale-all factor lon)
  (map (lambda (n) (* factor n)) lon))

;;; Q23.
;;; keep-short : Natural [List-of String] -> [List-of String]
;;;
;;; Keep only the strings in los whose length is at most max-len.
;;;
;;; (define (keep-short max-len los)
;;;   (filter (lambda (s) (<= (string-length s) max-len)) los))
;;;
;;; max-len : the maximum allowable string length (inclusive)
;;; los     : a list of strings
;;;
(check-expect (keep-short 3 '())                    '())
(check-expect (keep-short 3 '("hi" "bye" "hello"))  '("hi" "bye"))
(check-expect (keep-short 5 '("hi" "bye" "hello"))  '("hi" "bye" "hello"))

(define (keep-short max-len los)
  (filter (lambda (s) (<= (string-length s) max-len)) los))

;;; Q24.
;;; add-suffix : String [List-of String] -> [List-of String]
;;;
;;; Produce a list like los but with sfx appended to every string.
;;;
;;; (define (add-suffix sfx los)
;;;   (map (lambda (s) (string-append s sfx)) los))
;;;
;;; sfx : the string to append to each element
;;; los : a list of strings
;;;
(check-expect (add-suffix "!" '("hi" "bye"))  '("hi!" "bye!"))
(check-expect (add-suffix "?" '("really"))    '("really?"))
(check-expect (add-suffix "!" '())            '())

(define (add-suffix sfx los)
  (map (lambda (s) (string-append s sfx)) los))

;;; Q25.
;;; numbers-between : Number Number [List-of Number] -> [List-of Number]
;;;
;;; Keep only the numbers in lon that are strictly between lo and hi.
;;;
;;; (define (numbers-between lo hi lon)
;;;   (filter (lambda (n) (and (> n lo) (< n hi))) lon))
;;;
;;; lo  : the lower bound (exclusive)
;;; hi  : the upper bound (exclusive)
;;; lon : a list of numbers
;;;
(check-expect (numbers-between 3 8 '(1 3 5 7 9))   '(5 7))
(check-expect (numbers-between 0 5 '(0 1 2 3 4 5)) '(1 2 3 4))
(check-expect (numbers-between 3 8 '())            '())

(define (numbers-between lo hi lon)
  (filter (lambda (n) (and (> n lo) (< n hi))) lon))
