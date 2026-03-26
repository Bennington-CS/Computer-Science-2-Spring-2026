#lang racket
(require 2htdp/batch-io)

#|
create a file called ttt.txt
go to our etherpad

    ttt

    put up in a place
    where it's easy to see
    the cryptic admonishment
    t.t.t.

    when you feel how depressingly
    slowly you climb,
    it's well to remember that
    things take time.

    piet hein
|#

(read-file "ttt.txt")

; Difficult to count the number of lines
; Difficult to pick out a particular line

(define ttt (cons "TTT"
                  (cons ""
                        (cons "Put up in a place"
                              (cons "where it's easy to see" '())))))

; Easily count the number of lines
; Easily pick out a particular line

; Define a method called pick-3 that picks out the third line of ttt
(define (pick-3 f)
  (third f))

#;
(define (f alon) ; "alon" = "a list of names"
  (cond
    [(empty? alon) ...]
    [(cons? alon)
     (... (first alon) ... (rest (alon) ...))]))

;

; Write this recursively
(define (pick-n n f)
  (cond
    [(zero? n) (first f)]
    [else
      (pick-n (sub1 n) (rest f))]))
  
(pick-n 0 ttt)
(pick-n 3 ttt)

(define (pick-n-v2 n f)
  (cond
    [(equal? n 1) (first f)]
    [else
      (pick-n-v2 (sub1 n) (rest f))]))

(pick-n-v2 1 ttt)

(define (pick-n-v3 n f)
  (cond
    [(zero? (sub1 n)) (first f)]
    [else
      (pick-n-v3 (sub1 n) (rest f))]))

(define (pick-n-v4 n f)
  ;; input checking
  (if (not (positive? n)) #f
      ;; recursion
      (cond
        [(zero? (sub1 n)) (first f)]
        [else
          (pick-n-v4 (sub1 n) (rest f))])))

(pick-n-v4 0 ttt)

(read-lines "TTT.txt")
(read-words "TTT.txt")
(read-words/line "ttt.txt")

; Write a procedure that consumes TTT.txt as input, and uses read-words/line
; to count the number of lines and words, and produces (cons <words> (cons <lines> '()))

;;;
;;; Wishes
;;;

;;; number-of-words : List -> Number
;;; number-of-lines : List -> Number

(define (number-of-words low)
  (cond
    [(empty? low) 0]
    [else
      (add1 (number-of-words (rest low)))]))

(define (number-of-lines lol)
  (cond
    [(empty? lol) 0]
    [else
      (add1 (number-of-lines (rest lol)))]))

(define (number-of lst)
  (cond
    [(empty? lst) 0]
    [else
      (add1 (number-of (rest lst)))]))

(number-of '(1 2 3))
(number-of '((1 2 3) (4 5 6)))

; number-of-words-total : (List-of (List-of-Strings)) -> Number
; Counts all of the words is a list of lines
(define (number-of-words-total lol)
  (cond
    [(empty? lol) 0]
    [else
      (+ (number-of (first lol))
         (number-of-words-total (rest lol)))]))

(number-of-words-total '((1 2 3) (4 5 6))) ; 6
(number-of-words-total '((1 2 3) (4 5 6) (7 8 9))) ; 9

(define (number-of-words-and-lists lol)
  (cons (number-of-words-total lol) (cons (number-of lol) '())))

(number-of-words-and-lists '((1 2 3) (4 5 6)))
