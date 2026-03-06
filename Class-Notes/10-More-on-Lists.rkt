#lang htdp/bsl

#|
More in Lists: Lists of Lists; Lists as both input and output

Defintiion of a List:
- '()
- (cons _ List)

|#

;; Template for working with lists:
#;
(define (fn l)
  (cond
    [(empty? l) ...]
    [else (... (first l)
           ... (fn (rest l)) ...)]))


;; Design a function that takes a List of Numbers (lon), and returns a List of Numbers,
;; where each number is multiplied by 5.

;; multiply-by-five : List -> List
;;
;; Purpose:
;; Consume a List of Numbers, and produce a List of Numbers where each element is
;; multiplied by five.
;;
;; lon : a List of Numbers
;;
;; Tests:
;; (check-expect (multiply-by-five (cons 1 (cons 2 (cons 3 '()))))
;;               (cons 5 (cons 10 (cons 15 '()))))
;;
;; (check-expect (multiply-by-five '())
;;               '())
;;
;; Template:
;; (define (multiply-by-five lon)
;;  '())
;;
(define (multiply-by-five lon)
  (cond
    [(empty? lon) '()]
    [else (cons (* 5 (first lon))
                (multiply-by-five (rest lon)))]))

;; Suppose you have a List of Posns.  Consume the Posns, and produce a List of Posns where
;; you keep only those Posns whose x+y positions is < 10.
;;
;; '((1 1) (5 6)) -> '((1 1))
;;
;; filter-posns : List -> List
;;
;; Purpose:
;; Consume a List of Posns, and produce a List of Posns containing only those where
;; (+ x y) < 10
;;
;; lop: a List of Posns
;;
;; Tests:
;; (check-expect (filter-posns) ...)
;; (check-expect (filter-posns '()) '())
;;
;; Template:
;; (define (filter-posns lop)
;;   (cond
;;     [(empty? lop) '()]
;;     [else (... (first lop)
;;            ... (filter-posns (rest lop))) ...]))

(define (filter-posns lop)
  (cond
    [(empty? lop) '()]
    [else (if (< (+ (posn-x (first lop)) (posn-y (first lop))) 10)
              (cons (first lop) (filter-posns (rest lop)))
              (filter-posns (rest lop)))]))

(filter-posns '())
(filter-posns (cons (make-posn 5 6) '()))

;; Design a function called substitute-string, which consumes a List-of-Strings, a
;; target-string, a transformed-string, and returns a List-of-Strings with every
;; String of the form target-string with transformed-string.
;;
;; substitute-string : String String List-of-Strings -> List-of-Strings
;; Example:
;; (substitute-string "Hello" "Hi" (cons "Hello" (cons "Goodbye" '()))) ->
;; (cons "Hi" (cons "Goodbye" '()))
;;
;; Template:
;; (define (substitute-strings target-string transformed-string los)
;;   (cond
;;     [(empty? los) '()]
;;     [else (... (first los)
;;            ... (substitute-strings (rest los))) ...]))
;;
;; Purpose:
;; Consume a target-string, a transformed-string, and a List-of-Strings
;; Produce a List-of-Strings with every occurrence of target-string replaced
;; with transformed-string
;;
;; target-string      : the string to look for
;; transformed-string : the string to substitute
;; los                : a los
;;
(define (substitute-string target-string transformed-string los)
  (cond
    [(empty? los) '()]
    [else (cons
            (if (string=? (first los) target-string)
                transformed-string
                (first los))
            (substitute-string target-string transformed-string (rest los)))]))
         
(substitute-string "Hello" "Hi" (cons "Hello" (cons "Goodbye" '())))

