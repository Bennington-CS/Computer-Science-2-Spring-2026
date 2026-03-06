#|

Define: List of Strings; Non-empty List of Strings; Number

A List-of-strings is one of:
– '()
– (cons String List-of-strings)

An NEList-of-Strings (NELoS) is one of:
- (cons String '())
- (cons String List-of-Strings)

Or (but difficult to traverse):
- (cons String NEList-of-Strings)

An N is one of:
– 0
– (add1 N)

Template for N:

(define (num n)
  (cond
    [(zero? n) ...]
    [(positive? n)
     (... (num (sub1 n)) ...)]))

; An RD is one of:
; - String
; - (make-layer String RD)

List
Base: '()
Recursive call: (cons x lons)
Rest call: (rest los)

(define-struct layer [color doll])

Russian Dolls:
Base: String
Recursive Call: (make-layer color rusdoll)
Rest call: (layer-doll rusdoll)

(define (f-los los)
  (cond
    [(empty? los) ...]
    [else (... (first los)
           ... (f-los (rest los)))]))

(define (f-rd rusdoll)
  (cond
    [(string? rusdoll) ...]
    [else (... (layer-color russdol)
           ... (f-rd (layer-doll rusdoll)))])

|#

(define-struct layer [color doll])

(define rdY (make-layer "yellow" (make-layer "green" "red")))
(define rdP (make-layer "purple" (make-layer "black" (make-layer "pink" "peridot"))))

;; Design a function that calculates the depth of the doll
;; For example: (depth rdY) -> 3; (depth rdP) -> 4

(define (depth rusdoll)
  (cond
    [(string? rusdoll) 1]
    [else (add1 (depth (layer-doll rusdoll)))]))

(depth rdY)
(depth rdP)

(check-expect (depth rdP) 4)

;; Write a function that, given a layer, writes out all the colors
;; as a string with commas between them
;; For example: (colors rdY) -> "yellow, green, red"
;;              (colors rdP) -> "purple, black, pink, peridot")
;;              (colors "red") -> "red"

(define (colors rusdoll)
  (cond
    [(string? rusdoll) rusdoll]
    [else (string-append (layer-color rusdoll)
                         ", " 
                         (colors (layer-doll rusdoll)))]))

(colors rdY) ; "yellow, green, red"
(colors rdP) ; "purple, black, pink, peridot"
