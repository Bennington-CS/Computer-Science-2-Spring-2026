# Arbitrarily Large Data: A Summary of Sections 8—11

## CS2, Spring 2026

### What changed from CS1

Everything we built in the first course had a fixed size.  A `posn` has two fields.  A `game` struct has three.  You know at design time exactly how many pieces of data you're working with.

That's a real limitation.  A grocery list might have three items or thirty.  A game might track zero shots on screen, or a hundred.  We need a way to represent collections whose size we can't predict, and that's what this part of the book is about.  The answer is *lists*, and the technique for working with them is *self-referential data definitions*.

### Section 8: Lists

A list is built from two primitives: `'()` (the empty list, pronounced "empty") and `cons`, which sticks one item onto the front of an existing list.

```racket
'()                                       ; empty list
(cons "Mercury" '())                      ; one-item list
(cons "Venus" (cons "Mercury" '()))       ; two-item list
```

You build lists from the inside out.  Start with `'()` and wrap `cons` around it, one item at a time.  Think of each `cons` as a box with two compartments: `first` (the item you just added) and `rest` (the list you added it to).

To take a list apart, you have three operations.  `empty?` checks whether a list is `'()`.  `first` extracts the first item.  `rest` extracts everything after the first item.  That's it.

Here is the data definition you'll use again and again:

```racket
; A List-of-strings is one of:
; -- '()
; -- (cons String List-of-strings)
```

Look at that second clause: `List-of-strings` is defined *in terms of itself*.  This is a self-referential definition, and it's what lets lists hold arbitrarily many items.  It's not circular, because it has a base case (`'()`), a clause that doesn't refer back to itself.  Without that base case you could never stop building lists and never start processing them.

### Section 9: Designing with Self-Referential Data Definitions

The design recipe still works.  You just need to pay attention to how the self-reference in the data definition shows up at every step.

**Data definition.**  A valid self-referential data definition needs at least two clauses, and at least one clause that does *not* refer back to the definition.  Draw an arrow from the self-reference back to the name being defined.  You'll mirror that arrow in your template.

**Examples.**  Start with the base case (`'()`), then build up using the self-referential clause.  You need at least one example per clause, and you should include a list with two or more items so you can see the recursion work.

**Template.**  The template mirrors the data definition.  Two clauses in the definition produce two `cond` clauses in the template.  In the clause for `cons`, you write selector expressions (`first` and `rest`) and a *natural recursion*, which is a call to the function itself on `(rest ...)`.  That recursive call is the template's version of the arrow you drew in your data definition.

```racket
(define (fun-for-los alos)
  (cond
    [(empty? alos) ...]
    [else
     (... (first alos) ...
      ... (fun-for-los (rest alos)) ...)]))
```

**Filling in the template.**  Do the base case first; it's usually obvious.  For the recursive case, ask yourself three questions:

1. What does `(first alos)` give me?
2. What does the natural recursion compute?  (Read the purpose statement.)
3. How do I combine those two values to get my answer?

Question 2 requires a leap of faith.  You have to trust that the recursion already does the right thing for the rest of the list, because your purpose statement says it will.  This feels strange the first few times.  It works anyway.

This section also covers two variations.  *Non-empty lists* are lists guaranteed to have at least one item; their base case is `(cons X '())` instead of `'()`.  *Natural numbers* can also be understood as self-referential data: a natural number is either `0` or `(add1 n)` where `n` is a natural number, and functions over them follow the same template pattern with `sub1` playing the role of `rest`.

### Section 10: More on Lists

**Functions that produce lists.**  Not every function over a list returns a number or a Boolean.  Some return new lists.  The template is the same; what changes is what you put in the blanks.  For the base case you typically produce `'()`.  For the recursive case you `cons` a new item onto the result of the natural recursion.  A payroll function, for instance, might consume a list of hours and produce a list of wages, one wage per entry.

**Structures in lists.**  Lists can contain structures.  If your list items are `posn`s or some custom struct, you extract each item with `first` as before, then use the appropriate selectors to get at its fields.  When the item type is complex, put the per-item logic in a helper function instead of cramming everything into one definition.

**Lists in lists.**  When a file is represented as a list of lines and each line is a list of words, you get a list of lists.  The outer function recurs over lines; for each line, it calls a helper that recurs over words.  One data definition points to another, and each one gets its own function.  This is the principle of *one template per data definition*.

### Section 11: Design by Composition

**The `list` function.**  BSL+ gives you `list`, which is shorthand for nested `cons` expressions.  `(list 1 2 3)` means `(cons 1 (cons 2 (cons 3 '())))`.  Nothing about how lists work has changed: `first`, `rest`, `empty?`, `cons`, and all your data definitions are exactly the same.  You're just saving yourself from writing deeply nested `cons` chains, especially in examples and test cases.

**One function per task.**  When a problem involves several distinct tasks, design a separate function for each one.  A rendering function shouldn't also be computing collisions.  The guideline from the book: *design one function per task; formulate auxiliary function definitions for every dependency between quantities in the problem.*

**One template per data definition.**  When one data definition refers to another, each one drives its own function.  A function processing a list-of-lines calls a helper that processes a single line (itself a list).  This is the *wish list* approach: write down the signature, purpose, and header of the helper you need, assume it works, use it, and come back to design it later.

**Auxiliary functions that recur.**  Sometimes the combination step in a recursive function requires work that is itself recursive.  The textbook example is `rev` (reverse): to reverse a list, you reverse the rest, then add the first item to the *end* of that result.  "Add to the end" isn't a primitive operation, so you wish for a helper `add-at-end` and design it separately.

**Auxiliary functions that generalize.**  Sometimes the helper you need turns out to be more general than the specific problem at hand.  A function that removes a specific string from a list is a special case of a function that removes any given value.  The general version is usually no harder to write, and you can reuse it.

### How to think about these chapters

The same process runs through all four sections:

1. Write a self-referential data definition (with a base case).
2. Build the template from the data definition: one `cond` clause per data clause, natural recursion where there's a self-reference.
3. Fill in the base case.
4. For the recursive case, use the purpose statement to figure out what the recursion returns, then figure out how to combine it with `(first ...)`.
5. Test with the empty case, a one-element case, and a multi-element case.

If you're stuck on a function, go back to the data definition.  The structure of the data tells you the structure of the code.
