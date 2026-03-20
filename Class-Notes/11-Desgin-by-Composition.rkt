#lang htdp/bsl+

#|

List Abbreviations

(cons 1 (cons 2 (cons 3 '()))) <==> (list 1 2 3) <==> '(1 2 3)

(list) => '()
(list x) => (cons x '())
(list x y) => (cons x (cons y '()))

'((1 2) (3 4)) => 
(3 4) => (cons 3 (cons 4 '()))
(1 2) => (cons 1 (cons 2 '()))
|#

(cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '())) '()))

#|

- Design: One definition per task
- Convert a list of temperatures
  - Convert ONE temperature
  - Convert a LIST of temperatures

Wish:
- Write the definition to convert ONE temperature

Main:
- Write the definition  to convert a LIST of temperatures

|#

; (+ (* 9/5 c) 32)

; - Write the definition to convert ONE temperature
(define (c->f c)
  (+ (* 9/5 c) 32))

(c->f -40) ; -40
(c->f 0) ; 32
(c->f 100) ; 212

(define (convert-c-list-to-f temps)
  (cond
    [(empty? temps) '()]
    [else
      (cons (c->f (first temps))
                  (convert-c-list-to-f (rest temps)))]))


(convert-c-list-to-f '(-40 0 100))

#|

Insertion Sort

6 2 9 4

Step 1: Remove the first element
Step 2: Sort the rest
Step 3: Insert the first element at the right place

Step 1:
2 9 4
Step 2:
9 4 2
Step 3:
9 6 4 2

(list 6 2 9 4) .... 6 + (list 2 9 4)
-> Sort the rest, then insert 6

(list 2 9 4) .... 2 + (list 9 4)
-> Sort the rest, then insert 2

(list 9 4) .... 9 + (list 4)
-> Sort the rest, then insert 9

(list 4) .... '()
-> '()

-> insert 4 into '()
'(4)

-> insert 9 into '(4)
'(9 4)

-> insert 2 into '(9 4)
'(9 4 2)

-> insert 6 into '(9 4 2)
'(9 6 4 2)


Wish:
(insert n alon)
(sort> alon)

Write insert!

Insert 4 into '() -> '(4)

Insert 4 into '(5 3 1) -> '(5 4 3 1)

Insert 4 into '(1 3 5) -> '(1 3 5 4)

Insert 4 into '(1 5 3) -> '(1 5 4 3)

|#

;;;;;
;;;;; Insertion into a Sorted (Descending) List
;;;;;

;;;;
;;;; Data Definitions
;;;;

;;; A SortedList is one of:
;;; - '()
;;; - (cons Number SortedList)
;;; where the numbers  of SortedList are sorted from largest to smallest.
;;;
;;; Examples:
;;; - '()
;;; - '(5)
;;; - '(7 5 3 1)
;;;
;;; insert : Number SortedList -> SortedList
;;;
;;; Insert n into alon, a list of numbers sorted from largest to smallest, producing a new sorted list.
;;;
;;; Strategy: compare n to the first element.  If n is greater than or equal to it, n belongs at the
;;; front.  Otherwise, keep the first element and recurse on the rest of the list.
;;;
;;; Template:
;;; (define (insert n alon)
;;;   (cond
;;;     [(empty? alon) ...]
;;;     [else ... (first alon) ... (insert n (rest alon)) ...]))
;;;
;;; n    : the number to insert
;;; alon : a list of numbers sorted from largest to smallest
;;;
;;; Tests:
(check-expect (insert 4 '()) '(4))
(check-expect (insert 4 '(7 5 3 1)) '(7 5 4 3 1))
(check-expect (insert 8 '(7 5 3 1)) '(8 7 5 3 1))
(check-expect (insert 0 '(7 5 3 1)) '(7 5 3 1 0))
(check-expect (insert 5 '(7 5 3 1)) '(7 5 5 3 1))
(check-expect (insert 4 '(7)) '(7 4))
(check-expect (insert 9 '(7)) '(9 7))
(check-expect (insert 7 '(7 5 3 1)) '(7 7 5 3 1))
(check-expect (insert 3 '(7 5 3 1)) '(7 5 3 3 1))

;;;
;;; Definition
;;;
(define (insert n alon)
  (cond
    ;; If alon is empty, make n a list with n
    [(empty? alon) (list n)]
    ;; If n is larger than (first alon), cons n onto alon
    [(>= n (first alon)) (cons n alon)]
    ;; Otherwise, cons (first alon) onto the result of (insert ...)
    [else
     (cons (first alon) (insert n (rest alon)))]))


;;; insertion-sort : List-of-Numbers -> SortedList
;;;
;;; Sort a list of numbers from largest to smallest using insertion sort.  Consumes an unsorted list of
;;; numbers and produces a list sorted from largest to smallest.
;;;
;;; Strategy: follow the standard list template.  The empty list is already sorted.  Otherwise, sort the
;;; rest of the list recursively, then insert the first element into its proper position.
;;;
;;; (define (insertion-sort alon)
;;;   (cond
;;;     [(empty? alon) ...]
;;;     [else ... (first alon) ... (insertion-sort (rest alon)) ...]))
;;;
;;; alon : a list of numbers in any order
;;;
(check-expect (insertion-sort '()) '())
(check-expect (insertion-sort '(1)) '(1))
(check-expect (insertion-sort '(1 2 3)) '(3 2 1))
(check-expect (insertion-sort '(3 2 1)) '(3 2 1))
(check-expect (insertion-sort '(4 7 1 5 3)) '(7 5 4 3 1))
(check-expect (insertion-sort '(3 3 3)) '(3 3 3))
(check-expect (insertion-sort '(5 1 5 1)) '(5 5 1 1))
 
(define (insertion-sort alon)
  (cond
    [(empty? alon) '()]
    [else (insert (first alon) (insertion-sort (rest alon)))]))

;;; Data Definition
;;;
;;; A Player is a String
;;;
;;; A Team is one of:
;;; - (list Player Player) -> (cons Player (cons Player '()))
;;; - (cons Player Team)

(define duo (cons "Ada" (cons "Charles" '())))
(define trio (cons "Ada" (cons "Charles" (cons "Bob" '()))))

(define duo-list (list "Ada" "Charles"))
(define trio-list (list "Ada" "Charles" "Bob"))

;; Create a template for process-team:
;; process-team : Team -> Team
;;
;; (list "Ada" "Charles") -> (list "*Ada*" "*Charles*")

#;
(define (process-team t)
  (cond
    [(empty? (rest (rest t))) (... (first t) ... (second t) ...)]
    [else
     (... (first t) ... (process-team (rest t)) ...)]))

; A Shape is one of:
; – (cons Number (cons Number (cons Number '())))
; – (cons Number Shape)
; Interpretation: at least three y-coordinates; the x-coordinate for
; item i is (* i 20). The points are connected into a closed figure.

(define small-shape (list 10 30 10))          ; a triangle
(define big-shape   (list 10 30 30 10))       ; a trapezoid

; Signature:
#;
; Image Shape -> Image
; Draws the closed shape described by s into img.
(define (draw-shape img s)
  img)

;;;
;;; Helpers
;;;
(define MT (empty-scene 120 60))

(define (draw-seg img x1 y1 x2 y2)
  (scene+line img x1 y1 x2 y2 "red"))

#;
(define (draw-shape img s)
  (cond
    [(empty? (rest (rest (rest s))))
     ... (first s) ... (second s) ... (third s) ...img ...]
    [else
      ... (first s) ...(draw-shape img (rest s))]) ...)

(define (x-of s i) (* i 20))

(define (draw-shape img s)
  (cond
  [(empty? (rest (rest (rest s))))
   ;; Base case: connect 1->2, 2->3 3->1
     (draw-seg
       (draw-seg
         (draw-seg img
           (x-of s 0) (first s)
           (x-of s 1) (second s))
         (x-of s 1) (second s)
         (x-of s 2) (third s))
       (x-of s 2) (third s)
       (x-of s 0) (first s))]
   [else
    (draw-seg (draw-shape img (rest s))
              (x-of s 0) (first s)
              (x-of s 1) (second s))]))


