#lang racket
(require rackunit)

#lang htdp/bsl

#|

Class 2 Review:

- Construct lists using `cons` and `'()`
- Interpret and draw box-and-arrow diagrams
- Understand why the list data definition is self-referential
- Derive a recursive function template from the data definition
- Understand `contains-flatt?`
- Explain why recursion terminates on lists

Class 3 Goals:

- Understand recursion as a consequence of data structures, not a programming trick
- Combinators, and how to discover them
- Challenges with non-empty lists

What makes recursion necessary rather than optional?
- Arbitrary size
  - Indefinite number of elements
  - If you have e.g., 10 elements, you can do it without recursion
  - In an indefinite number, you to recurse in order to get through all of them
- Self-reference in the data description
  - Base case
  - Recursion is mirroring what is in the data description

Suppose you are designing a function that counts the number of strings in a collection 
of unknown size.  Show the data definition for List-of-strings.

; List-of-strings is one of:
; – '()
; – (cons String List-of-strings)

In groups: generate four examples of List-of-strings of increasing size:
|#
'()
(cons "Madina" '())
(cons "cat" (cons "bird" '()))
(cons "Adler" (cons "cat" (cons "bird" '())))

#;
(define (fn-for-los alos)
  (cond
    [(empty? alos) ...]
    [else
      (... (first alos) ... (fn-for-los (rest alos)) ...)]))

#;
(define (how-many alos)
  (cond
    [(empty? alos) ...]
    [else
      (... (first alos) ... (how-many (rest alos)) ...)]))

(define (how-many alos)
  (cond
    [(empty? alos) 0]
    [else
      (add1 (how-many (rest alos)))]))
    
; Purpose: To count the number of elements in a List-of-strings (using add1)
(how-many (cons "Adler" (cons "cat" (cons "bird" '()))))

#|

What if you don't see the combinator?
- "Combinator": an expression that combines the values available in a cond branch
  to produce a result

Problem:
Design a function `cat` that uses recursion on a List-of-strings to consume every
string in the List-of-strings, and return a single string that is the concatenation
of all those strings.

Purpose: To consume a List-of-strings, and produce a concatenation of every string
in that list-of-strings.

Signature:
cat : List-of-strings -> String

Data Definition:
alos : Representing a list of strings

Examples:
(cons "a" (cons "b" '())) -> "ab"
'() -> ""

Tests:
(check-expect ...)
(check-expect ...)

Header:
(define (cat alos)
  ...
  "")

Template:
(define (cat alos)
  (cond
    [(empty? alos) ...]
    [else
      (... (first alos) ... (cat (rest alos)) ...)]))

|#

(define (cat alos)
  (cond
    [(empty? alos) ""]
    [else
      (string-append (first alos) (cat (rest alos)))]))

(cat (cons "a" (cons "b" '()))) ; "ab"
(cat (cons "b" (cons "a" '()))) ; "ba"
(cat '()) ; ""

#|

Table: input, first, rest, cat(rest) cat(los)

input,                    first, rest,          cat(rest)  cat(los)
(cons "a" (cons "b" '())  "a"    (cons "b" '()) "b"        "ab"
(cons "b" '())            "b"    -              ""         "b"
'()                       -      -              -          ""
  
; List-of-temperatures (lot) is one of:
; '()
; (cons CTemperature List-of-temperatures)

; An NEList-of-temperatures (nelot) is one of:
; (cons CTemperature '())
; (cons CTemperature) NEList-of-temperature)

Problem: Generate an average of CTemperatures

Average: Sum + Number-of-elements

(define (average/lot alot) ...)

Wish:
(define (sum/lot alot) ...)
(define (how-many/lot alot) ...)

Main:
(define (average/lot alot)
  (/ (sum/lot alot)
     (how-many/lot alot)))
|#

#;
(define (sum/lot alot)
  )

#;
(define (how-many/lot alot)
  )
