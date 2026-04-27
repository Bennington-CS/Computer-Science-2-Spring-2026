#lang htdp/isl+

;;;;;
;;;;; Lambda Worksheet
;;;;; ISL+ (Intermediate Student with Lambda)
;;;;;

#|
A lambda expression creates an anonymous function -- a function with no
name.  The syntax is:

  (lambda (param 0) body)

This is equivalent to writing (define (f param 0) body) and then
referring to f by name -- except that no name is introduced.  The main
use of lambda is as an argument to an abstraction like map, filter,
foldr, build-list, andmap, ormap, and sort.  Instead of defining a
helper separately, you write it inline.

Your job in this worksheet is to work through four kinds of exercises:

  Part 1 -- Reading     what does this expression evaluate to?
  Part 2 -- Translating rewrite a named-helper call using lambda
  Part 3 -- Writing     write an abstraction call with lambda from scratch
  Part 4 -- Closures    lambda that captures a parameter from outer scope

Fill in each blank where you see 0 .
Do not change the check-expects.
|#

;;;;
;;;; Part 1 — Reading Lambda Expressions
;;;;
;;;; What does each expression evaluate to?
;;;; Replace the 0 in each check-expect with the correct value.
;;;;

;;; Q1.
(check-expect ((lambda (x) (* x x)) 5)
              0)

;;; Q2.
(check-expect ((lambda (s) (string-length s)) "hello")
              0)

;;; Q3.
(check-expect ((lambda (x y) (+ x y)) 3 4)
              0)

;;; Q4.
(check-expect (map (lambda (x) (+ x 1)) '(10 20 30))
              0)

;;; Q5.
(check-expect (filter (lambda (n) (> n 3)) '(1 2 3 4 5))
              0)

;;; Q6.
(check-expect (map (lambda (s) (string-append s "!")) '("hi" "bye"))
              0)

;;; Q7.
(check-expect (build-list 5 (lambda (i) (* i i)))
              0)

;;; Q8.
(check-expect (foldr (lambda (n acc) (+ n acc)) 0 '(1 2 3 4))
              0)

;;; Q9.
(check-expect (andmap (lambda (n) (even? n)) '(2 4 6))
              0)

;;; Q10.
(check-expect (ormap (lambda (n) (even? n)) '(1 3 4 7))
              0)

;;;;
;;;; Part 2 — Translating to Lambda
;;;;
;;;; Each question gives a named helper and a call that uses it.
;;;; Rewrite the call as a single expression using lambda.
;;;; Do not use the helper name in your answer.
;;;;

;;; Q11.
;;; Original:
;;;   (define (double n) (* 2 n))
;;;   (map double '(1 2 3 4 5))
;;;
;;; Your answer -- fill in the lambda:
(check-expect (map 0 '(1 2 3 4 5))
              '(2 4 6 8 10))

;;; Q12.
;;; Original:
;;;   (define (negative? n) (< n 0))
;;;   (filter negative? '(3 -1 0 -4 2))
;;;
;;; Your answer:
(check-expect (filter 0 '(3 -1 0 -4 2))
              '(-1 -4))

;;; Q13.
;;; Original:
;;;   (define (index->square i) (* i i))
;;;   (build-list 6 index->square)
;;;
;;; Your answer:
(check-expect (build-list 6 0)
              '(0 1 4 9 16 25))

;;; Q14.
;;; Original:
;;;   (define (longer? s t) (> (string-length s) (string-length t)))
;;;   (sort '("hi" "elephant" "cat") longer?)
;;;
;;; Your answer:
(check-expect (sort '("hi" "elephant" "cat") 0)
              '("elephant" "cat" "hi"))

;;; Q15.
;;; Original:
;;;   (define (add-exclamation s) (string-append s "!"))
;;;   (map add-exclamation '("wow" "yes" "ok"))
;;;
;;; Your answer:
(check-expect (map 0 '("wow" "yes" "ok"))
              '("wow!" "yes!" "ok!"))

;;;;
;;;; Part 3 — Writing Lambda from Scratch
;;;;
;;;; Write the complete expression using the abstraction named and a lambda.
;;;; Replace the 0 with your answer.
;;;;

;;; Q16.  Use map.
;;; Produce a list of the lengths of the strings in the list.
(check-expect (0 '("apple" "fig" "mango"))
              '(5 3 5))

;;; Q17.  Use filter.
;;; Keep only the strings that are longer than 3 characters.
(check-expect (0 '("hi" "hello" "ok" "world"))
              '("hello" "world"))

;;; Q18.  Use build-list.
;;; Produce the list (list 0 3 6 9 12) -- multiples of 3, starting from 0.
(check-expect (0)
              '(0 3 6 9 12))

;;; Q19.  Use foldr.
;;; Count how many numbers in the list are greater than 10.
;;; Hint: your lambda should add 1 to acc when n > 10, otherwise add 0.
(check-expect (0 '(5 12 3 20 8 11))
              3)

;;; Q20.  Use andmap.
;;; Determine whether every string in the list starts with "a".
;;; Hint: (substring s 0 1) gives the first character of s as a string.
(check-expect (0 '("apple" "avocado" "apricot"))
              #true)

;;;;
;;;; Part 4 — Lambda Over a Parameter (Closures)
;;;;
;;;; When a helper needs two pieces of information but the abstraction
;;;; passes only one, the extra piece must come from the surrounding scope.
;;;; Lambda captures it automatically.
;;;;
;;;; Earlier in the course you handled this with local.  Lambda is a
;;;; more direct way to express the same idea.
;;;;

;;; Q21.
;;; What does this evaluate to?  Fill in the check-expect.
(define THRESHOLD 10)

(check-expect (filter (lambda (n) (< n THRESHOLD)) '(5 10 15 3 20))
              0)

;;; Q22.
;;; The two definitions below behave identically.
;;; Fill in the blank in Version B.
;;;
;;; Version A -- using local
#;
(define (scale-all factor lon)
  (local [(define (scale-one n) (* factor n))]
    (map scale-one lon)))
;;;
;;; Version B -- using lambda (fill in the blank)
#;
(define (scale-all factor lon)
  (map (lambda (n) (* 0 n)) lon))

;;; Q23.
;;; Design keep-short using filter and lambda.
;;; It consumes a maximum length and a list of strings, and keeps only
;;; those strings whose length is at most max-len.
;;;
;;; keep-short : Natural [List-of String] -> [List-of String]
;;;
;;; (define (keep-short max-len los)
;;;   (filter (lambda (s) (<= 0 0)) los))
;;;
;;; max-len : the maximum allowable string length (inclusive)
;;; los     : a list of strings
;;;
(check-expect (keep-short 3 '())                    '())
(check-expect (keep-short 3 '("hi" "bye" "hello"))  '("hi" "bye"))
(check-expect (keep-short 5 '("hi" "bye" "hello"))  '("hi" "bye" "hello"))

(define (keep-short max-len los)
  0)

;;; Q24.
;;; Design add-suffix using map and lambda.
;;; It consumes a suffix string and a list of strings, and produces
;;; a new list with the suffix appended to every string.
;;;
;;; add-suffix : String [List-of String] -> [List-of String]
;;;
;;; (define (add-suffix sfx los)
;;;   (map (lambda (s) (string-append 0 0)) los))
;;;
;;; sfx : the string to append to each element
;;; los : a list of strings
;;;
(check-expect (add-suffix "!" '("hi" "bye"))  '("hi!" "bye!"))
(check-expect (add-suffix "?" '("really"))    '("really?"))
(check-expect (add-suffix "!" '())            '())

(define (add-suffix sfx los)
  0)

;;; Q25.
;;; Design numbers-between using filter and lambda.
;;; It consumes lo and hi (both numbers) and a list of numbers, and keeps
;;; only those strictly between lo and hi.
;;;
;;; numbers-between : Number Number [List-of Number] -> [List-of Number]
;;;
;;; (define (numbers-between lo hi lon)
;;;   (filter (lambda (n) (and (> n 0) (< n 0))) lon))
;;;
;;; lo  : the lower bound (exclusive)
;;; hi  : the upper bound (exclusive)
;;; lon : a list of numbers
;;;
(check-expect (numbers-between 3 8 '(1 3 5 7 9))   '(5 7))
(check-expect (numbers-between 0 5 '(0 1 2 3 4 5)) '(1 2 3 4))
(check-expect (numbers-between 3 8 '())             '())

(define (numbers-between lo hi lon)
  0)
