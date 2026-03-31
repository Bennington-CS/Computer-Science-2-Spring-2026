;;;;;
;;;;; Worm Game with Food (Exercise 219)
;;;;; A world program
;;;;;

(require 2htdp/image)
(require 2htdp/universe)

#|
Exercise 219.  Equip your program from exercise 218 with food.  At any
point in time, the box should contain one piece of food.  A piece of food
is the same size as a worm segment.  When the worm's head is located at
the same position as the food, the worm eats it: the tail is extended by
one segment and a new piece of food appears at a different location.

This requires a new world-state data representation that combines the worm
with the food's current position, new handler functions that delegate to
the existing worm helpers, and a tick handler that manages both movement
and the eating/food-creation cycle.

--- How food-create works (figure 80) ---

food-create consumes a Posn (the position to avoid) and produces a Posn
(the new food location).  It generates a random candidate using
(make-posn (random MAX) (random MAX)).  If the candidate happens to land
on the avoided position, it calls itself again with a fresh random
candidate --- this is generative recursion, because each recursive call
creates brand-new data rather than decomposing the input.  Since MAX > 1,
the grid has at least 4 cells, so the probability of matching shrinks with
each attempt and termination is practically guaranteed.

food-check-create is the helper that does the actual comparison: if the
candidate equals the avoided position, try again; otherwise return it.

not=-1-1? is a testing predicate for use with check-satisfied.  Because
food-create is non-deterministic (it calls random), we cannot use
check-expect.  Instead we verify that food-create, given (make-posn 1 1),
always produces a Posn that is NOT (1, 1).
|#

;;;; To code:
;;;; physical and graphical constants (including FOOD-IMG, MAX)
;;;; game struct (worm, food) — the new world state
;;;;
;;;; Wishes (carried forward from exercise 217/218):
;;;; grid->px          -- convert grid coordinate to pixel center
;;;; new-head          -- compute the next head position
;;;; all-but-last      -- drop the last element of a non-empty list
;;;; render-segments   -- place every segment image onto a scene
;;;;
;;;; New wishes:
;;;; food-create       -- generate a random food position avoiding a given Posn
;;;; food-check-create -- helper for food-create (generative recursion)
;;;;
;;;; Definitions (carried forward, now operating on Game):
;;;; game-render       (to-draw)
;;;; game-tock         (on-tick)   — moves worm, manages eating + new food
;;;; game-steer        (on-key)
;;;; game-over?        (stop-when) — wall or self collision
;;;; game-render-final (stop-when last-scene)
;;;; game-main         (main)

;;;;
;;;; Constants
;;;;

;;; Physical constants

(define SEG-SIZE 10)                  ; diameter of one worm segment (pixels)
(define GRID-W  30)                   ; game board width  (in segments)
(define GRID-H  30)                   ; game board height (in segments)
(define BOARD-W (* GRID-W SEG-SIZE))  ; game board width  (pixels)
(define BOARD-H (* GRID-H SEG-SIZE))  ; game board height (pixels)
(define MAX GRID-W)                   ; upper bound for random food placement
                                      ; assumes a square grid (GRID-W = GRID-H)

;;; Graphical constants

(define SEG-IMG  (circle (/ SEG-SIZE 2) "solid" "red"))
(define FOOD-IMG (circle (/ SEG-SIZE 2) "solid" "green"))
(define BG       (empty-scene BOARD-W BOARD-H))
(define FONT-SIZE 14)
(define FONT-COLOR "black")

;;;;
;;;; Data Definitions
;;;;

;;; A Direction is one of:
;;; - "up"
;;; - "down"
;;; - "left"
;;; - "right"
;;; interpretation: the direction the worm moves on each tick

;;; A NEList-of-Posns is one of:
;;; - (list Posn)
;;; - (cons Posn NEList-of-Posns)
;;; interpretation: a non-empty list of grid positions; the first element
;;;   is the head and the remaining elements are tail segments from front
;;;   to back.  "Connected" means each segment differs from its predecessor
;;;   by at most one unit in one direction.

(define-struct worm [segs dir])
;;; A Worm is a structure:
;;;   (make-worm NEList-of-Posns Direction)
;;; interpretation: (make-worm segs dir) is a worm whose segments are at
;;;   the grid positions in segs (head first) moving in direction dir.

(define-struct game [worm food])
;;; A Game is a structure:
;;;   (make-game Worm Posn)
;;; interpretation: (make-game w f) is a game state where w is the worm
;;;   and f is the grid position of the single piece of food.
;;;
;;; Examples:
;;;   (make-game (make-worm (list (make-posn 5 5)) "right")
;;;              (make-posn 10 10))
;;;     -- a one-segment worm heading right, food at (10, 10)
;;;   (make-game (make-worm (list (make-posn 7 5) (make-posn 6 5)
;;;                               (make-posn 5 5)) "right")
;;;              (make-posn 20 15))
;;;     -- a three-segment worm heading right, food at (20, 15)

;;; Shorthand examples

(define WORM-1
  (make-worm (list (make-posn 5 5)) "right"))

(define WORM-3
  (make-worm (list (make-posn 7 5)
                   (make-posn 6 5)
                   (make-posn 5 5)) "right"))

(define WORM-5
  (make-worm (list (make-posn 7 5)
                   (make-posn 6 5)
                   (make-posn 5 5)
                   (make-posn 4 5)
                   (make-posn 3 5)) "right"))

(define GAME-1 (make-game WORM-1 (make-posn 10 10)))
(define GAME-3 (make-game WORM-3 (make-posn 20 15)))

;;;;
;;;; Wishes (helper functions)
;;;;

;;; grid->px : Number -> Number
;;;
;;; Convert a grid coordinate to the pixel coordinate of that cell's
;;; center.  Consumes a grid coordinate and produces a pixel coordinate.
;;;
;;; (define (grid->px g)
;;;   (... g ... SEG-SIZE ...))
;;;
;;; g : a non-negative integer
;;;
(check-expect (grid->px 0) (/ SEG-SIZE 2))
(check-expect (grid->px 1) (+ SEG-SIZE (/ SEG-SIZE 2)))
(check-expect (grid->px 5) (+ (* 5 SEG-SIZE) (/ SEG-SIZE 2)))

(define (grid->px g)
  (+ (* g SEG-SIZE) (/ SEG-SIZE 2)))

;;; new-head : Posn Direction -> Posn
;;;
;;; Compute the grid position one step from the given position in the
;;; given direction.  Consumes a Posn and a Direction, produces a Posn.
;;;
;;; (define (new-head p dir)
;;;   (cond
;;;     [(string=? dir "up")    (make-posn ... ...)]
;;;     [(string=? dir "down")  (make-posn ... ...)]
;;;     [(string=? dir "left")  (make-posn ... ...)]
;;;     [(string=? dir "right") (make-posn ... ...)]))
;;;
;;; p   : a Posn in grid coordinates
;;; dir : a Direction
;;;
(check-expect (new-head (make-posn 5 5) "right") (make-posn 6 5))
(check-expect (new-head (make-posn 5 5) "left")  (make-posn 4 5))
(check-expect (new-head (make-posn 5 5) "up")    (make-posn 5 4))
(check-expect (new-head (make-posn 5 5) "down")  (make-posn 5 6))

(define (new-head p dir)
  (cond
    [(string=? dir "up")    (make-posn (posn-x p) (sub1 (posn-y p)))]
    [(string=? dir "down")  (make-posn (posn-x p) (add1 (posn-y p)))]
    [(string=? dir "left")  (make-posn (sub1 (posn-x p)) (posn-y p))]
    [(string=? dir "right") (make-posn (add1 (posn-x p)) (posn-y p))]))

;;; all-but-last : NEList-of-X -> List-of-X
;;;
;;; Produce a list like the given one but without its last element.
;;; When the input has exactly one element, the result is '().
;;;
;;; (define (all-but-last lox)
;;;   (cond
;;;     [(empty? (rest lox)) ...]
;;;     [else (cons (first lox) ... (all-but-last (rest lox)) ...)]))
;;;
;;; lox : a non-empty list
;;;
(check-expect (all-but-last (list "a"))         '())
(check-expect (all-but-last (list "a" "b"))     (list "a"))
(check-expect (all-but-last (list "a" "b" "c")) (list "a" "b"))

(define (all-but-last lox)
  (cond
    [(empty? (rest lox)) '()]
    [else (cons (first lox) (all-but-last (rest lox)))]))

;;; render-segments : List-of-Posns Image -> Image
;;;
;;; Place a SEG-IMG at each grid position in the list onto the given
;;; scene.  Consumes a list of Posns and a scene, produces an Image.
;;;
;;; (define (render-segments segs scene)
;;;   (cond
;;;     [(empty? segs) scene]
;;;     [else (place-image SEG-IMG
;;;             (grid->px (posn-x (first segs)))
;;;             (grid->px (posn-y (first segs)))
;;;             (render-segments (rest segs) scene))]))
;;;
;;; segs  : a List-of-Posns (may be empty during recursion)
;;; scene : an Image onto which segments are drawn
;;;
(check-expect (render-segments '() BG) BG)
(check-expect (render-segments (list (make-posn 0 0)) BG)
              (place-image SEG-IMG (grid->px 0) (grid->px 0) BG))
(check-expect (render-segments (list (make-posn 1 0) (make-posn 0 0)) BG)
              (place-image SEG-IMG (grid->px 1) (grid->px 0)
                (place-image SEG-IMG (grid->px 0) (grid->px 0) BG)))

(define (render-segments segs scene)
  (cond
    [(empty? segs) scene]
    [else (place-image SEG-IMG
            (grid->px (posn-x (first segs)))
            (grid->px (posn-y (first segs)))
            (render-segments (rest segs) scene))]))

;;; food-create : Posn -> Posn
;;;
;;; Produce a random grid position that is different from the given
;;; position.  Consumes a Posn (the position to avoid, typically the
;;; worm's head) and produces a Posn.  Uses generative recursion: each
;;; recursive call generates a brand-new random candidate rather than
;;; decomposing the input.
;;;
;;; (define (food-create p)
;;;   (food-check-create p (make-posn (random MAX) (random MAX))))
;;;
;;; p : a Posn to avoid
;;;
;;; Because food-create uses random, we cannot use check-expect.
;;; Instead we use check-satisfied with not=-1-1? (see below).
;;;
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)

(define (food-create p)
  (food-check-create
   p (make-posn (random MAX) (random MAX))))

;;; food-check-create : Posn Posn -> Posn
;;;
;;; If the candidate position equals the avoided position, try again by
;;; calling food-create; otherwise return the candidate.  This is the
;;; comparison step of the generative recursion in food-create.
;;;
;;; (define (food-check-create p candidate)
;;;   (if (equal? p candidate) (food-create p) candidate))
;;;
;;; p         : the Posn to avoid
;;; candidate : a randomly generated Posn
;;;
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

;;; not=-1-1? : Posn -> Boolean
;;;
;;; Produce #true if the given Posn is NOT (1, 1).  This predicate is
;;; used only for testing food-create with check-satisfied: it verifies
;;; that food-create, given (make-posn 1 1), never returns that same
;;; position.
;;;
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

;;;;
;;;; Worm helpers (carried forward from exercises 217/218)
;;;;

;;; worm-move : Worm -> Worm
;;;
;;; Move the worm one segment in its current direction by adding a new
;;; head and dropping the last tail segment.  Consumes and produces a
;;; Worm.
;;;
(check-expect (worm-move WORM-1)
              (make-worm (list (make-posn 6 5)) "right"))
(check-expect (worm-move WORM-3)
              (make-worm (list (make-posn 8 5) (make-posn 7 5) (make-posn 6 5))
                         "right"))
(check-expect (worm-move (make-worm (list (make-posn 5 5)) "up"))
              (make-worm (list (make-posn 5 4)) "up"))
(check-expect (worm-move (make-worm (list (make-posn 5 5) (make-posn 5 4)) "down"))
              (make-worm (list (make-posn 5 6) (make-posn 5 5)) "down"))

(define (worm-move w)
  (make-worm (cons (new-head (first (worm-segs w)) (worm-dir w))
                   (all-but-last (worm-segs w)))
             (worm-dir w)))

;;; worm-grow : Worm -> Worm
;;;
;;; Grow the worm by one segment: add a new head in the current
;;; direction but keep all existing segments (do not drop the last one).
;;; This is used when the worm eats food.  Consumes and produces a Worm.
;;;
;;; (define (worm-grow w)
;;;   (make-worm (cons (new-head ...) (worm-segs w))
;;;              (worm-dir w)))
;;;
;;; w : a Worm
;;;
;;; Why is this interpretation of "eating" easy to design?  Because the
;;; new head naturally moves to the food's position (the worm was heading
;;; there), and keeping the full segment list means the tail extends by
;;; exactly one segment — the old head position stays in place rather
;;; than being vacated.
;;;
(check-expect (worm-grow WORM-1)
              (make-worm (list (make-posn 6 5) (make-posn 5 5)) "right"))
(check-expect (worm-grow WORM-3)
              (make-worm (list (make-posn 8 5)
                               (make-posn 7 5)
                               (make-posn 6 5)
                               (make-posn 5 5))
                         "right"))

(define (worm-grow w)
  (make-worm (cons (new-head (first (worm-segs w)) (worm-dir w))
                   (worm-segs w))
             (worm-dir w)))

;;; worm-steer : Worm KeyEvent -> Worm
;;;
;;; Change the worm's direction in response to an arrow key.  All other
;;; keys are ignored.  Consumes a Worm and a KeyEvent, produces a Worm.
;;;
(check-expect (worm-steer WORM-3 "up")
              (make-worm (worm-segs WORM-3) "up"))
(check-expect (worm-steer WORM-3 "down")
              (make-worm (worm-segs WORM-3) "down"))
(check-expect (worm-steer WORM-3 "left")
              (make-worm (worm-segs WORM-3) "left"))
(check-expect (worm-steer WORM-1 "right")
              (make-worm (worm-segs WORM-1) "right"))
(check-expect (worm-steer WORM-3 "a") WORM-3)
(check-expect (worm-steer WORM-3 " ") WORM-3)

(define (worm-steer w ke)
  (cond
    [(string=? ke "up")    (make-worm (worm-segs w) "up")]
    [(string=? ke "down")  (make-worm (worm-segs w) "down")]
    [(string=? ke "left")  (make-worm (worm-segs w) "left")]
    [(string=? ke "right") (make-worm (worm-segs w) "right")]
    [else w]))

;;; worm-hit-wall? : Worm -> Boolean
;;;
;;; Has the worm's head moved outside the grid?  Produces #true when the
;;; head's x or y coordinate is out of bounds.
;;;
(check-expect (worm-hit-wall? WORM-3) #false)
(check-expect (worm-hit-wall? (make-worm (list (make-posn -1 5)) "left"))  #true)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 5 -1)) "up"))    #true)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 30 5)) "right")) #true)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 5 30)) "down"))  #true)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 0 0)) "up"))     #false)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 29 29)) "down")) #false)

(define (worm-hit-wall? w)
  (or (< (posn-x (first (worm-segs w))) 0)
      (>= (posn-x (first (worm-segs w))) GRID-W)
      (< (posn-y (first (worm-segs w))) 0)
      (>= (posn-y (first (worm-segs w))) GRID-H)))

;;; worm-hit-self? : Worm -> Boolean
;;;
;;; Has the worm's head landed on one of its own tail segments?
;;; Uses member? to search the tail.
;;;
(check-expect (worm-hit-self? WORM-1) #false)
(check-expect (worm-hit-self? WORM-3) #false)
(check-expect (worm-hit-self?
               (make-worm (list (make-posn 5 5) (make-posn 6 5) (make-posn 5 5))
                          "left"))
              #true)

(define (worm-hit-self? w)
  (member? (first (worm-segs w)) (rest (worm-segs w))))

;;; worm-eating? : Worm Posn -> Boolean
;;;
;;; Is the worm about to eat the food?  Produces #true when the next
;;; head position (one step in the current direction) equals the food
;;; position.  Consumes a Worm and a Posn (the food), produces a Boolean.
;;;
;;; (define (worm-eating? w food)
;;;   (equal? (new-head (first (worm-segs w)) (worm-dir w)) food))
;;;
;;; w    : a Worm
;;; food : a Posn (the food's grid position)
;;;
(check-expect (worm-eating? (make-worm (list (make-posn 5 5)) "right")
                            (make-posn 6 5))
              #true)
(check-expect (worm-eating? (make-worm (list (make-posn 5 5)) "right")
                            (make-posn 10 10))
              #false)
(check-expect (worm-eating? (make-worm (list (make-posn 5 5)) "up")
                            (make-posn 5 4))
              #true)

(define (worm-eating? w food)
  (equal? (new-head (first (worm-segs w)) (worm-dir w)) food))

;;;;
;;;; Definitions
;;;;

;;; game-render : Game -> Image
;;;
;;; Produce an image of the game: the food and all worm segments drawn on
;;; the background.  The food is drawn first (underneath), then the worm
;;; segments on top.  Consumes a Game, produces an Image.
;;;
;;; (define (game-render g)
;;;   (render-segments (worm-segs (game-worm g))
;;;     (place-image FOOD-IMG
;;;       (grid->px (posn-x (game-food g)))
;;;       (grid->px (posn-y (game-food g)))
;;;       BG)))
;;;
;;; g : a Game
;;;
(check-expect (game-render GAME-1)
              (render-segments (list (make-posn 5 5))
                (place-image FOOD-IMG
                  (grid->px 10) (grid->px 10)
                  BG)))
(check-expect (game-render GAME-3)
              (render-segments (list (make-posn 7 5)
                                    (make-posn 6 5)
                                    (make-posn 5 5))
                (place-image FOOD-IMG
                  (grid->px 20) (grid->px 15)
                  BG)))

(define (game-render g)
  (render-segments (worm-segs (game-worm g))
    (place-image FOOD-IMG
      (grid->px (posn-x (game-food g)))
      (grid->px (posn-y (game-food g)))
      BG)))

;;; game-tock : Game -> Game
;;;
;;; Advance the game by one tick.  If the worm is about to eat the food,
;;; grow the worm (add a new head without dropping the last segment) and
;;; place new food at a random location that differs from the new head.
;;; Otherwise, move the worm normally (add head, drop last segment) and
;;; keep the food where it is.  Consumes and produces a Game.
;;;
;;; (define (game-tock g)
;;;   (if (worm-eating? (game-worm g) (game-food g))
;;;       (make-game (worm-grow (game-worm g))
;;;                  (food-create ...))
;;;       (make-game (worm-move (game-worm g))
;;;                  (game-food g))))
;;;
;;; g : a Game
;;;

;; Normal movement (no eating): food stays, worm slides forward
(check-expect (game-tock (make-game (make-worm (list (make-posn 5 5)) "right")
                                    (make-posn 20 20)))
              (make-game (make-worm (list (make-posn 6 5)) "right")
                         (make-posn 20 20)))

;; Three-segment worm, no eating
(check-expect (game-tock (make-game WORM-3 (make-posn 20 20)))
              (make-game (make-worm (list (make-posn 8 5)
                                         (make-posn 7 5)
                                         (make-posn 6 5))
                                   "right")
                         (make-posn 20 20)))

;; Eating: worm grows, new food is created.  check-random synchronizes
;; the random calls on both sides so they produce the same food position.
(check-random (game-tock (make-game (make-worm (list (make-posn 5 5)) "right")
                                    (make-posn 6 5)))
              (make-game (make-worm (list (make-posn 6 5) (make-posn 5 5)) "right")
                         (food-create (make-posn 6 5))))

(define (game-tock g)
  (if (worm-eating? (game-worm g) (game-food g))
      (make-game (worm-grow (game-worm g))
                 (food-create
                  (first (worm-segs (worm-grow (game-worm g))))))
      (make-game (worm-move (game-worm g))
                 (game-food g))))

;;; game-steer : Game KeyEvent -> Game
;;;
;;; Steer the worm via the arrow keys.  Delegates to worm-steer and
;;; keeps the food unchanged.  Consumes a Game and a KeyEvent, produces
;;; a Game.
;;;
;;; (define (game-steer g ke)
;;;   (make-game (worm-steer (game-worm g) ke) (game-food g)))
;;;
;;; g  : a Game
;;; ke : a KeyEvent (a String)
;;;
(check-expect (game-steer GAME-3 "up")
              (make-game (make-worm (worm-segs WORM-3) "up")
                         (make-posn 20 15)))
(check-expect (game-steer GAME-3 "a") GAME-3)

(define (game-steer g ke)
  (make-game (worm-steer (game-worm g) ke) (game-food g)))

;;; game-over? : Game -> Boolean
;;;
;;; Should the game stop?  Produces #true when the worm has hit a wall
;;; or run into itself.  Consumes a Game, produces a Boolean.
;;;
;;; (define (game-over? g)
;;;   (or (worm-hit-wall? (game-worm g))
;;;       (worm-hit-self? (game-worm g))))
;;;
;;; g : a Game
;;;
(check-expect (game-over? GAME-3) #false)
(check-expect (game-over?
               (make-game (make-worm (list (make-posn -1 5)) "left")
                          (make-posn 10 10)))
              #true)
(check-expect (game-over?
               (make-game
                (make-worm (list (make-posn 5 5) (make-posn 6 5) (make-posn 5 5))
                           "left")
                (make-posn 10 10)))
              #true)

(define (game-over? g)
  (or (worm-hit-wall? (game-worm g))
      (worm-hit-self? (game-worm g))))

;;; game-render-final : Game -> Image
;;;
;;; Produce the final scene when the game ends.  Overlays a message in
;;; the lower left explaining why the game stopped ("worm hit wall" or
;;; "worm hit self") onto the normal game rendering.  Consumes a Game,
;;; produces an Image.
;;;
;;; (define (game-render-final g)
;;;   (place-image/align (text ... FONT-SIZE FONT-COLOR)
;;;                      1 (- BOARD-H 1) "left" "bottom"
;;;                      (game-render g)))
;;;
;;; g : a Game
;;;
(check-expect
 (game-render-final
  (make-game (make-worm (list (make-posn -1 5)) "left") (make-posn 10 10)))
 (place-image/align (text "worm hit wall" FONT-SIZE FONT-COLOR)
              1 (- BOARD-H 1) "left" "bottom"
              (game-render
               (make-game (make-worm (list (make-posn -1 5)) "left")
                          (make-posn 10 10)))))

(check-expect
 (game-render-final
  (make-game
   (make-worm (list (make-posn 5 5) (make-posn 6 5) (make-posn 5 5)) "left")
   (make-posn 10 10)))
 (place-image/align (text "worm hit self" FONT-SIZE FONT-COLOR)
              1 (- BOARD-H 1) "left" "bottom"
              (game-render
               (make-game
                (make-worm (list (make-posn 5 5) (make-posn 6 5) (make-posn 5 5))
                           "left")
                (make-posn 10 10)))))

(define (game-render-final g)
  (place-image/align
   (text (if (worm-hit-wall? (game-worm g))
             "worm hit wall"
             "worm hit self")
         FONT-SIZE FONT-COLOR)
   1 (- BOARD-H 1) "left" "bottom"
   (game-render g)))

;;;;
;;;; Main
;;;;

;;; game-main : Worm Number Boolean -> Number
;;;
;;; Launch the worm game with food.  The first argument is the initial
;;; worm state, the second is the clock tick rate in seconds, and the
;;; third controls whether big-bang opens a separate state-inspector
;;; window (useful for debugging).  Returns the length of the worm when
;;; the game ends.
;;;
;;; Example:
;;;   (game-main WORM-3 1/3 #false)
;;;
(define (game-main worm rate show-state?)
  (length
   (worm-segs
    (game-worm
     (big-bang (make-game worm
                          (food-create (first (worm-segs worm))))
       [on-tick  game-tock rate]
       [on-key   game-steer]
       [to-draw  game-render]
       [stop-when game-over? game-render-final]
       [state    show-state?])))))

;; To play:
;; (game-main WORM-3 1/5 #false)
