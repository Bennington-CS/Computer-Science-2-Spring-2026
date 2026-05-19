#lang racket
(require rackunit)

;;;;;
;;;;; KVICK SÖRT
;;;;; Reference Implementation
;;;;;

;;;;
;;;; Problem Statement
;;;;

;;;
;;; Sort a list of numbers into ascending order using the quicksort algorithm,
;;; following the IDEA-style flat-pack instructions:
;;;
;;;   1. Pick a pivot from the list (the diagram uses a die — i.e. a random
;;;      element; for a deterministic implementation we use the first element).
;;;   2. Draw a line at the pivot's height.
;;;   3. Find everything taller (greater than) the pivot, and everything
;;;      shorter-or-equal.
;;;   4-5. Partition: shorter-or-equal go to the left group, taller go to the
;;;      right group; the pivot sits between them.
;;;   6. Recursively KVICK SÖRT each group.
;;;
;;; Base case: an empty list is already sorted.

;;;;
;;;; Data Definitions
;;;;

;;; A LON (List of Numbers) is one of:
;;; - '()
;;; - (cons Number LON)
;;; interp.  a list of numbers
;;;
;;; Template:
;;; (define (lon-fn a-lon)
;;;   (cond [(empty? a-lon) ...]
;;;         [else (... (first a-lon) ... (lon-fn (rest a-lon)) ...)]))

;;;;
;;;; Definitions
;;;;

;;; quicksort : LON -> LON
;;;
;;; Sort a list of numbers in ascending order using the KVICK SÖRT algorithm.
;;; A pivot is chosen at random (per the die in step 1 of the diagram); the
;;; list of numbers shorter-or-equal to the pivot is sorted recursively and
;;; placed before the pivot; the list of numbers taller than the pivot is
;;; sorted recursively and placed after.
;;;
;;; (define (quicksort a-lon)
;;;   (cond [(empty? a-lon) ...]
;;;         [else (... (first a-lon) ... (quicksort (rest a-lon)) ...)]))
;;;
;;; a-lon : a list of numbers to be sorted
;;;
(define (quicksort a-lon)
  (cond [(empty? a-lon) '()]
        [else
         (local [;; Step 1: roll the die — pick a random pivot from a-lon
                 (define pivot (list-ref a-lon (random (length a-lon))))
                 ;; Everything except one occurrence of the pivot
                 (define others (remove pivot a-lon))
                 ;; Step 3-4: partition the rest of the list around the pivot
                 (define smaller (filter (lambda (n) (<= n pivot)) others))
                 (define larger  (filter (lambda (n) (>  n pivot)) others))]
           ;; Step 5-6: recursively sort each side, pivot sits between them
           (append (quicksort smaller)
                   (list pivot)
                   (quicksort larger)))]))

;; Note:
;; (list-ref '(a b c d) 2)
;; (remove 'a '(a b c))

;;; Tests
(check-equal? (quicksort '())
              '()
              "Empty list is already sorted")
(check-equal? (quicksort '(7))
              '(7)
              "Single-element list is already sorted")
(check-equal? (quicksort '(1 2 3 4 5))
              '(1 2 3 4 5)
              "Already-sorted list stays sorted")
(check-equal? (quicksort '(5 4 3 2 1))
              '(1 2 3 4 5)
              "Reverse-sorted list gets sorted")
(check-equal? (quicksort '(3 1 4 1 5 9 2 6 5 3 5))
              '(1 1 2 3 3 4 5 5 5 6 9)
              "Duplicates are preserved")
(check-equal? (quicksort '(-2 0 -5 3 -1))
              '(-5 -2 -1 0 3)
              "Negatives and zero sort correctly")

(quicksort '(5 2 6 1 7))
