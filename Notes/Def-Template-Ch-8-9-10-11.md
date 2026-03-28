# Data Definitions and Templates: Sections 8—11

## CS2, Designing Worlds, Spring 2026

Here are all the data definitions from Sections 8—11, each paired with
its characteristic function template.  The arrow (-->) in each data
definition marks the self-reference; the recursive call in the template
goes in the same spot.


## Section 8: Lists

### List-of-names

The first self-referential data definition in the book.

```racket
; A List-of-names is one of:
; -- '()
; -- (cons String List-of-names)  -->
; interpretation a list of invitees, by last name
```

Template:

```racket
(define (fun-for-lon alon)
  (cond
    [(empty? alon) ...]
    [(cons? alon)
     (... (first alon) ...
      ... (fun-for-lon (rest alon)) ...)]))
```

The first `cond` clause handles `'()`.  The second handles `cons`
and contains `(first alon)` (the item) and a recursive call on
`(rest alon)` (the self-reference).


### 3LON (fixed-size, not self-referential)

For comparison: a list with a known, fixed number of items.
No self-reference, no recursion.

```racket
; A 3LON is a list of three numbers:
;   (cons Number (cons Number (cons Number '())))
; interpretation a point in 3-dimensional space
```

No template shown.  This is just a compound value with a known shape,
like a struct with three fields.


## Section 9: Designing with Self-Referential Data Definitions

### List-of-strings

Same shape as List-of-names.  You'll see this pattern a lot.

```racket
; A List-of-strings is one of:
; -- '()
; -- (cons String List-of-strings)  -->
```

Template:

```racket
(define (fun-for-los alos)
  (cond
    [(empty? alos) ...]
    [else
     (... (first alos) ...
      ... (fun-for-los (rest alos)) ...)]))
```


### List-of-temperatures

Same structure, different item type.  The template is identical.

```racket
(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; A List-of-temperatures is one of:
; -- '()
; -- (cons CTemperature List-of-temperatures)  -->
```

Template:

```racket
(define (fun-for-lot alot)
  (cond
    [(empty? alot) ...]
    [(cons? alot)
     (... (first alot) ...
      ... (fun-for-lot (rest alot)) ...)]))
```


### NEList-of-temperatures (non-empty lists)

The base case here is a one-element list, not `'()`.  Both clauses
use `cons`, so the condition checks `(rest ne-l)` instead of `ne-l`
itself.

```racket
; An NEList-of-temperatures is one of:
; -- (cons CTemperature '())
; -- (cons CTemperature NEList-of-temperatures)  -->
; interpretation non-empty lists of Celsius temperatures
```

Template:

```racket
(define (fun-for-net ne-l)
  (cond
    [(empty? (rest ne-l)) (... (first ne-l) ...)]
    [else
     (... (first ne-l) ...
      ... (fun-for-net (rest ne-l)) ...)]))
```

Watch the condition: it's `(empty? (rest ne-l))`, not `(empty? ne-l)`.
Both clauses use `(first ne-l)` because even the base case has an item.


### N (natural numbers)

Natural numbers are self-referential data too.  `add1` is the
constructor, `sub1` is the selector, `zero?` and `positive?` are the
predicates.

```racket
; An N is one of:
; -- 0
; -- (add1 N)  -->
; interpretation represents the counting numbers
```

Template:

```racket
(define (fun-for-n n)
  (cond
    [(zero? n) ...]
    [(positive? n)
     (... (fun-for-n (sub1 n)) ...)]))
```

Compare this to the list template.  `sub1` plays the role of `rest`
and `zero?` plays the role of `empty?`.


### RD (Russian Dolls)

Self-referential, but uses a struct instead of `cons`.  The
self-reference lives in the `doll` field.

```racket
(define-struct layer [color doll])

; An RD is one of:
; -- String
; -- (make-layer String RD)  -->
; interpretation the innermost doll is a String (its color);
;   each layer wraps a color around another doll
```

Template:

```racket
(define (fun-for-rd an-rd)
  (cond
    [(string? an-rd) ...]
    [(layer? an-rd)
     (... (layer-color an-rd) ...
      ... (fun-for-rd (layer-doll an-rd)) ...)]))
```

The recursive call is on `(layer-doll an-rd)` because that's where
the self-reference is.  `(layer-color an-rd)` is just a String, so
no recursion there.


## Section 10: More on Lists

### List-of-numbers (functions that produce lists)

Same shape as always.  What's new in 10.1 is that the function's
*output* is also a list.

```racket
; A List-of-numbers is one of:
; -- '()
; -- (cons Number List-of-numbers)  -->
```

Template:

```racket
(define (fun-for-lon alon)
  (cond
    [(empty? alon) ...]
    [else
     (... (first alon) ...
      ... (fun-for-lon (rest alon)) ...)]))
```

When the function produces a list, the base case is typically `'()` and
the recursive case typically uses `cons`:

```racket
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs))
                (wage* (rest whrs)))]))
```


### Work / Low (structures in lists)

When list items are structures, the data definition for the list refers
to a separate data definition for the item.  Each gets its own template.

```racket
(define-struct work [employee rate hours])
; A Work is a structure:
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name
;   with the pay rate r and the number of hours h

; A Low (list of works) is one of:
; -- '()
; -- (cons Work Low)  -->
```

Template for Low (the list):

```racket
(define (fun-for-low an-low)
  (cond
    [(empty? an-low) ...]
    [(cons? an-low)
     (... (fun-for-work (first an-low)) ...
      ... (fun-for-low (rest an-low)) ...)]))
```

Template for Work (the item):

```racket
(define (fun-for-work w)
  (... (work-employee w) ...
   ... (work-rate w) ...
   ... (work-hours w) ...))
```

The list template calls out to the item template through a helper.
One data definition, one template, one function.


### LN / List-of-list-of-strings (lists in lists)

When list items are themselves lists, you get two layers of
self-reference.  Each layer has its own data definition and template.

```racket
; An LN is one of:
; -- '()
; -- (cons Los LN)  -->
; interpretation a list of lines, each is a list of Strings

; A Los (List-of-strings) is one of:
; -- '()
; -- (cons String Los)  -->
```

Template for LN (outer list):

```racket
(define (fun-for-lln lls)
  (cond
    [(empty? lls) ...]
    [else
     (... (line-processor (first lls)) ...
      ... (fun-for-lln (rest lls)) ...)]))
```

Template for Los (inner list, one line):

```racket
(define (line-processor ln)
  (cond
    [(empty? ln) ...]
    [else
     (... (first ln) ...
      ... (line-processor (rest ln)) ...)]))
```

The outer template calls `line-processor` on `(first lls)` instead
of inlining selectors, because that first item is itself a list with
its own recursive structure.


## Section 11: Design by Composition

No new data definitions here.  Section 11 introduces BSL+'s `list`
shorthand and guidelines for composing functions.  The data definitions
and templates from Sections 8—11 still apply; what changes is how you
organize multiple functions around them.

Two guidelines:

  1. Design one function per task.
  2. Design one template per data definition.

When one data definition refers to another, each gets its own function.
That's the wish list approach: write down the signature, purpose, and
header of the helper you need, use it, and come back to design it later.


## The pattern

Every self-referential data definition in Sections 8—11 has this shape:

```
; A Thing is one of:
; -- <base case>
; -- <compound with ... Thing ...>  -->
```

And every template mirrors it:

```racket
(define (fun-for-thing t)
  (cond
    [<base-case?> ...]
    [else
     (... <selectors> ...
      ... (fun-for-thing <self-ref-selector>) ...)]))
```
