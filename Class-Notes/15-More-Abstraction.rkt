#|
Abstraction: in terms of values; in terms of functions

; A List-of-strings is one of:
; -- '()
; -- (cons String List-of-strings)

; A List-of-numbers is one of:
; -- '()
; -- (cons Number List-of-numbers)

; A [List-of ITEM] is one of:
; -- '()
; -- (cons ITEM [List-of ITEM])

[List-of-Strings]
[List-of-Numbers]
[List-of-Posns]
... [List-of-Items]

|#

(define (all-passing? lon)
  (cond
    [(empty? lon) #true]
    [else (and (>= (first lon) 60)
               (all-passing? (rest lon)))]))

(define (all-positive? lon)
  (cond
    [(empty? lon) #true]
    [else (and (> (first lon) 0)
               (all-positive? (rest lon)))]))

; Abstract these two functions: return #t if a predicate function returns
; true on all instances of a list

(define (all-satisfy? pred lst)
  (cond
    [(empty? lst) #true]
    [else (pred (first lst)
                (all-satisfy? pred (rest lst)))]))

; Number -> Number
; converts one Celsius temperature to Fahrenheit
(check-expect (C2F 0)   32)
(check-expect (C2F 100) 212)
(check-expect (C2F -40) -40)
(define (C2F c)
  (+ (* 9/5 c) 32))

; List-of-numbers -> List-of-numbers
; converts a list of Celsius temperatures to Fahrenheit
(check-expect (cf* (list 0 100 -40)) (list 32 212 -40))

(define (cf* l)
  (cond
    [(empty? l) '()]
    [else
     (cons (C2F (first l))
           (cf* (rest l)))]))

(define-struct IR [name price])
; An IR (Inventory Record) is a structure:
;   (make-IR String Number)
; interpretation: the name and price of one item in inventory
; An Inventory is one of:
;   - '()
;   - (cons IR Inventory)
; interpretation: a list of inventory records

(check-expect (names (list (make-IR "doll" 21) (make-IR "bear" 13)))
              (list "doll" "bear"))

(define (names lst)
  (cond
    [(empty? lst) '()]
    [else
      (cons (IR-name (first lst))
            (names (rest lst)))]))

(check-expect (map1 (list 0 100 -40) C2F)
              (list 32 212 -40))
(check-expect (map1 (list (make-IR "doll" 21) (make-IR "bear" 13)) IR-name)
              (list "doll" "bear"))

(define (map1 lst fn)
  (cond
    [(empty? lst) '()]
    [else
      [cons (fn (first lst))
            (map1 (rest lst) fn)]]))

(define (only-even lst)
  (cond
    [(empty? lst) '()]
    [(not (even? (first lst))) (only-even (rest lst))]
    [else
      (cons (first lst) (only-even (rest lst)))]))

(define (low lst)
  (cond
    [(empty? lst) '()]
    [(not (<= 10 (first lst))) (low (rest lst))]
    [else
      (cons (first lst) (low (rest lst)))]))

(define (filter1 lst pred)
  (cond
    [(empty? lst) '()]
    [(not (pred (first list))) (filter1 (rest lst))]
    [else
      (cons (first lst) (filter1 (rest lst)))]))

