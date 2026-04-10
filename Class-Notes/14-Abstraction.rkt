;;; has-a? : List-of-strings -> Boolean
;;; does los contain "a"?
(define (has-a? los)
  (cond
    [(empty? los) #false]
    [else
     (or (string=? (first los) "a")
         (has-a? (rest los)))]))

;;; has-b? : List-of-strings -> Boolean
;;; does los contain "b"?
(define (has-b? los)
  (cond
    [(empty? los) #false]
    [else
     (or (string=? (first los) "b")
         (has-b? (rest los)))]))

(has-a? (list "a" "b"))
(has-a? (list "b"))

;;; has-b? : List-of-strings -> Boolean
;;; does los contain "b"?
(define (has-char? char los)
  (cond
    [(empty? los) #false]
    [else
      (or (string=? (first los) char)
          (has-char? char (rest los)))]))

(has-char? "c" (list "a" "b"))
(has-char? "c" (list "a" "c" "b"))

;;; underpaid : List-of-numbers Number -> List-of-numbers
;;; select salaries in lon that are below threshold t
(check-expect (underpaid '() 50000) '())
(check-expect (underpaid (list 40000 60000 30000) 50000)
              (list 40000 30000))

(define (underpaid lon t)
  (cond
    [(empty? lon) '()]
    [else
      (cond
        [(< (first lon) t)
         (cons (first lon)
               (underpaid (rest lon) t))]
        [else
          (underpaid (rest lon) t)])]))

;;; overpaid : List-of-numbers Number -> List-of-numbers
;;; select salaries in lon that are above threshold t
(check-expect (overpaid '() 50000) '())
(check-expect (overpaid (list 40000 60000 30000) 50000)
              (list 60000))

(define (overpaid lon t)
  (cond
    [(empty? lon) '()]
    [else
      (cond
        [(> (first lon) t)
         (cons (first lon)
               (overpaid (rest lon) t))]
        [else
          (overpaid (rest lon) t)])]))

;;; filter-salaries : Fn List Number -> List-of-numbers
(check-expect (filter-salaries < (list 40000 60000 30000) 50000)
              (list 40000 30000))
(check-expect (filter-salaries > (list 40000 60000 30000) 50000)
              (list 60000))
(check-expect (filter-salaries = (list 50000 60000 50000) 50000)
              (list 50000 50000))
(check-expect (filter-salaries < '() 50000) '())

;; Don't do this
;; (define (filter-salaries fn lon t)
;;   (cond
;;     [(empty? lon) '()]
;;     [else
;;       (cond
;;         [(equal? fn ">")
;;          (if (> (first lon) t) (cons (first lon)
;;                                  (filter-salaries fn (rest lon) t)))]
;;         [(equal? fn ">")
;;          (if (> (first lon) t) (cons (first lon)
;;                              (filter-salaries fn (rest lon) t)))]
;;          [else
;;            (filter-salaries fn (rest lon) t)])]))

(define (filter-salaries fn lon t)
  (cond
    [(empty? lon) '()]
    [else
      (cond
        [(fn (first lon) t)
         (cons (first lon)
               (filter-salaries fn (rest lon) t))]
        [else
          (filter-salaries fn (rest lon) t)])]))

(filter-salaries = (list 10000 20000 30000) 20000)

; A List-of-strings is one of:
; -- '()
; -- (cons String List-of-strings)

; A List-of-numbers is one of:
; -- '()
; -- (cons Number List-of-numbers)
;

; A [List-of item] is one of:
; -- '()
; -- (cons item [List-of item])

;;; all-passing? : List-of-numbers -> Boolean
;;; are all scores in lon at least 60?
(check-expect (all-passing? '()) #true)
(check-expect (all-passing? (list 80 90 75)) #true)
(check-expect (all-passing? (list 80 55 75)) #false)

(define (all-passing? lon)
  (cond
    [(empty? lon) #true]
    [else (and (>= (first lon) 60)
               (all-passing? (rest lon)))]))

;;; all-positive? : List-of-numbers -> Boolean
;;; are all numbers in lon greater than 0?
(check-expect (all-positive? '()) #true)
(check-expect (all-positive? (list 3 7 1)) #true)
(check-expect (all-positive? (list 3 -2 1)) #false)

(define (all-positive? lon)
  (cond
    [(empty? lon) #true]
    [else (and (> (first lon) 0)
               (all-positive? (rest lon)))]))

;;; all-satisfy? : pred List-of-numbers -> Boolean
;;; do all numbers in lon satisfy the test pred?
(check-expect (all-satisfy? positive? '()) #true)
(check-expect (all-satisfy? positive? (list 3 7 1)) #true)
(check-expect (all-satisfy? positive? (list 3 -2 1)) #false)

(define (all-satisfy? pred lon)
  (cond
    [(empty? lon) #t]
    [else
      (and (pred (first lon))
           (all-satisfy? pred (rest lon)))]))


(all-satisfy? positive? '(1 2 3 4))
(all-satisfy? positive? '(-1 2 3 4))
(all-satisfy? all-passing? '(50 60 70))
