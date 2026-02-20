#lang htdp/bsl

#|
Class 2: Friday, February 20, 2026: Lists (8-8.3)

Goals:
- Construct lists using `cons` and `'()`
- Interpret and draw box-and-arrow diagrams
- Understand why the list data definition is self-referential
- Derive a recursive function template from the data definition
- Understand `contains-flatt?`
- Explain why recursion terminates on lists

What is a list?

'() - Empty list — smallest possible list
What sort of thing is '()
Is it a function?
Is it a structure?
Is it a value?

Answer: '() is a: value
- cannot be reduced further (like 5, "Hi")
- can be returned as the result of a function

Exercise:
Construct a list, containing: "Earth", "Venus", "Mercury"
- have to use `cons`
- no shortcuts
|#

(cons "Mercury" '()) ; '("Mercury")
(cons "Veenus" (cons "Mercury" '())) ; '("Veenus" "Mercury")
(cons "Earth" (cons "Venus" (cons "Mercury" '()))) ; '("Earth" "Venus" "Mercury")

#|
n element -> n cons cells -> one '()

Diagramming Lists:
- nested-box diagrams
- box-and-arrow-chain diagrams

|#

(first (cons "Earth" (cons "Venus" '()))) ; "Earth"
(rest  (cons "Earth" (cons "Venus" '()))) ; '("Venus")

#|

Definition of a list:

; A List-of-names is one of:
; – '()
; – (cons String/Whatever List-of-names)


- Self-referential, but not circular
- Two elements:
  - Base case: '()
  - Recursive case: (cons ...)

|#

; Template (embodiment of the definition of alon above)
;#
(define (f alon) ; "alon" = "a list of names"
  (cond
    [(empty? alon) ...]
    [(cons? alon)
     (... (first alon) ... (rest (alon) ...))]))

(empty? '()) ; #t
(empty? (cons "Venus" '())) ; #f

#|

contains-flatt? : alon -> Boolean
If "Flatt" is an element in alon, then #t
If it is an empty list, then #f
Otherwise, #f

- Write tests or this using check-expect

|#
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect (contains-flatt? (cons "A" (cons "George" '()))) #false)

(define (contains-flatt? alon) ; "alon" = "a list of names"
  (cond
    [(empty? alon) #f]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

(contains-flatt? '()) ; #f
(contains-flatt? (cons "Flatt" '()))
(contains-flatt? (cons "A" (cons "George" '()))) #false) ; #f

#|
(contains-flatt? (cons "A" (cons "George" '())))
(or #false (contains-flatt? (cons "George" '())))

(contains-flatt? (cons "George" '()))
(or #false (contains-flatt? '())))

(contains-flat? '())
#f

Base case: '()
Recursive case: (or ...)
|#

#|
- Rewrite this defintion so that it works with any string.
- Construct a definition such that it *counts* the number of elements.
|#

(define (contains-string? s alon) ; "alon" = "a list of names"
  (cond
    [(empty? alon) #f]
    [(cons? alon)
     (or (string=? (first alon) s)
         (contains-string? (rest alon)))]))
