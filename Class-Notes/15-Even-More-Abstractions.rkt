; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))
	
  ; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))
;;
;; Design tabulate, which is the abstraction of the two functions in
;; figure 92. When tabulate is properly designed, use it to define a
;; tabulation function for sqr and tan.

;; Step 1: Write down the similarities between the two definitions.
;; Step 2: Abstract.  That is, replace the contents of the corresponding
;; code with new names and add these names to the formal parameter list.
;; Step 3: Validate by running tests (check-expect).

;; Step 1: 
;; These functions are similar, except for the names of the definitions
;; and the sin vs sqrt functions.

;; Step 2:
(define (tabulate f n)
  (cond
    [(zero? n) (list (f 0))]
    [else
      (cons
        (f n)
        (tabulate f (sub1 n)))]))

(tabulate sin 5)
(tabulate sqrt 5)

;; Step 3:
(check-within (tabulate sin 2)
              (list (sin 2) (sin 1) (sin 0)) 0.001)
(check-expect (tabulate add1 3)
              (list (add1 3) (add1 2) (add1 1) (add1 0)))

(define (tab-sin-v2 n)
  (tabulate sin n))
(check-within (tab-sin-v2 2) (tab-sin 2))

;; Exercise 251
;; [List-of Number] -> Number
;; computes the sum of ; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

;; Design fold1, which is the abstraction of the two functions in figure 93.

;; Step 1: Compare the definitions:
;; The differences are two: first, the base case value (0 vs 1);
;; second, the combining function (+ vs *)
;;
;; Everything else is identical: the cond structure, the empty? check,
;; the (first l), and the recursive call on (rest l)

;; Step 2: Abstract - replace each difference with a parameter
(define (fold1 lst base f)
  (cond
    [(empty? lst) base]
    [else
      (f (first lst)
         (fold1 (rest lst) base f))]))

(fold1 (list 3 4 5) 0 +)
(fold1 (list 3 4 5) 1 *)
(fold1 (list 3 4 5) 1 /)

;; Step 3
(define (sum-v2 l)
  (fold1 l 0 +))
(check-expect (sum-v2 (list 3 4 5)) 12)

(define (product-v2 l)
  (fold1 l 1 *))
(check-expect (product-v2 (list 3 4 5)) 60)

;; Problem: Write a signature for map1
(define (map1 l f)
  (cond
    [(empty? l) '()]
    [else
     (cons (f (first l))
           (map1 (rest l) f))]))

(map1 (list 1 2 3 4 5) add1)
(map1 (list 1 2 3 4 5) positive?)
(map1 (list 1 2 3 4 5) even?)


; map1: List Fn -> List
; map1: List [Number -> Number) -> List
; map1: List [String -> String) -> List
; map1: List [Like, anything -> Like, anything) -> List
; 
; These are problematic: 1. List should be List-of;
;                        2. Fn that is a formal parameter can be 
;                           all sorts of different types
;
; map1: [List-of X] [X -> Y] -> [List-ofY]

;;;; Exercise 253
;;;; Each signature describes a class of functions. Give at least one
;;;; ISL example for each.

;; Exercise 253. Each of these signatures describes a class of functions:
;;
;;     ; [Number -> Boolean]
;;     ; [Boolean String -> Boolean]
;;     ; [Number Number Number -> Number]
;;     ; [Number -> [List-of Number]]
;;     ; [[List-of Number] -> Boolean]
;;
;; Describe these collections with at least one example from ISL.

;; [Number -> Boolean] : even?
;; [Boolean String -> Boolean] : string=?
(define (bool-and-nonempty? b s)
  (and (boolean? b) (> (string-length s) 0)))

(bool-and-nonempty? #t "a")

; +

(tab-sin 5)

(empty? (list 1 2 3 4 5))

;; Exercise 252
;; Function 1
; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))

;; Function 2	
; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

#|

Similarities/Differences between product and image*:

           Product:                      Image*:
consumes:  [List-of Number]              [List-of Posn]
base case: 1                             (empty-scene 100 100) <=> emt
combine:   *                             place-dot
produces:  Number                        Image

Three differences in the functions not including the output:
the base case, the combining function, and the input

*: Consumes two Numbers, and produces a Number
place-dot: Consumes a Posn, and an Image, and produces an Image
|#

(define (fold2 lst base combine)
  (cond
    [(empty? lst) base]
    [else
      (combine (first lst)
               (fold2 (rest lst) base combine))]))

(fold2 (list 3 4 5) 1 *)
(fold2 (list (make-posn 30 40) (make-posn 10 20)) emt place-dot)
