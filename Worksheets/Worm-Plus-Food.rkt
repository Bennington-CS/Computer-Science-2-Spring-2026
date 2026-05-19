#lang htdp/isl+
;;;;;
;;;;; Worm-Plus-Food: Two-Player Worm Game with 5 Food Pellets
;;;;; A world program
;;;;;

(require 2htdp/image)
(require 2htdp/universe)

#|
Two-player variant of the worm game from exercise 219.

Player 1 (red worm)  steers with the arrow keys.
Player 2 (blue worm) steers with W/A/S/D.

;; CHANGED: There are now 5 pieces of food on the board at all times
;; (instead of 1).  When a worm eats a pellet, that pellet is replaced
;; by a new one at a random location; the other 4 pellets are unchanged.

The game ends when either worm hits a wall, hits itself, or hits the
body of the other worm.

--- Design changes from worm-plus (1-pellet version) ---

Data:
  The food field of Game changes from Posn to List-of-Posn (always
  exactly NUM-FOOD elements).

Rendering:
  render-food folds FOOD-IMG over the food list onto the scene, just
  as render-segments does for worm segments.  game-render calls
  render-food instead of a single place-image.

worm-eating?:
  Now takes a List-of-Posn and returns the eaten Posn (or #false).
  Uses ormap/filter to find which pellet (if any) the worm's next
  head lands on.

food-remove:
  New helper.  Removes one specific Posn from a List-of-Posn.

game-tock:
  When a worm eats, food-remove drops the eaten pellet and
  food-create adds a replacement.  The rest of the logic is
  identical to worm-plus.

game-main:
  Initial food is built by food-create-list, which creates NUM-FOOD
  distinct random pellets.
|#

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

;; CHANGED: number of food pellets on the board at any time
(define NUM-FOOD 5)

;;; Graphical constants

(define SEG-IMG1  (circle (/ SEG-SIZE 2) "solid" "red"))    ; player 1 body colour
(define HEAD-IMG1 (circle (/ SEG-SIZE 2) "solid" "pink"))   ; player 1 head colour
(define SEG-IMG2  (circle (/ SEG-SIZE 2) "solid" "blue"))   ; player 2 body colour
(define HEAD-IMG2 (circle (/ SEG-SIZE 2) "solid" "turquoise")) ; player 2 head colour
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
;;;   to back.

(define-struct worm [segs dir])
;;; A Worm is a structure:
;;;   (make-worm NEList-of-Posns Direction)
;;; interpretation: (make-worm segs dir) is a worm whose segments are at
;;;   the grid positions in segs (head first) moving in direction dir.

(define-struct game [worm1 worm2 food])
;;; A Game is a structure:
;;;   (make-game Worm Worm List-of-Posn)
;;; interpretation: (make-game w1 w2 fs) is a game state where w1 is the
;;;   red (player-1) worm, w2 is the blue (player-2) worm, and fs is a
;;;   list of exactly NUM-FOOD grid positions, one per food pellet.
;;;
;; CHANGED: food field is now a List-of-Posn (was a single Posn).

;;; Shorthand examples

(define WORM1-1
  (make-worm (list (make-posn 5 5)) "right"))

(define WORM1-3
  (make-worm (list (make-posn 7 5)
                   (make-posn 6 5)
                   (make-posn 5 5)) "right"))

(define WORM2-1
  (make-worm (list (make-posn 20 20)) "left"))

(define WORM2-3
  (make-worm (list (make-posn 18 20)
                   (make-posn 19 20)
                   (make-posn 20 20)) "left"))

;; CHANGED: food field is now a list of 5 Posns (was a single Posn)
(define FOOD-5
  (list (make-posn 10 10)
        (make-posn 12 12)
        (make-posn 14 14)
        (make-posn 16 16)
        (make-posn 18 18)))

(define GAME-1 (make-game WORM1-1 WORM2-1 FOOD-5))
(define GAME-3 (make-game WORM1-3 WORM2-3 FOOD-5))

;;;;
;;;; Wishes (helper functions, carried forward unchanged)
;;;;

;;; grid->px : Number -> Number
;;;
;;; Convert a grid coordinate to the pixel coordinate of that cell's
;;; center.
;;;
(check-expect (grid->px 0) (/ SEG-SIZE 2))
(check-expect (grid->px 1) (+ SEG-SIZE (/ SEG-SIZE 2)))
(check-expect (grid->px 5) (+ (* 5 SEG-SIZE) (/ SEG-SIZE 2)))

(define (grid->px g)
  (+ (* g SEG-SIZE) (/ SEG-SIZE 2)))

;;; new-head : Posn Direction -> Posn
;;;
;;; Compute the grid position one step from p in direction dir.
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
;;;
(check-expect (all-but-last (list "a"))         '())
(check-expect (all-but-last (list "a" "b"))     (list "a"))
(check-expect (all-but-last (list "a" "b" "c")) (list "a" "b"))

(define (all-but-last lox)
  (cond
    [(empty? (rest lox)) '()]
    [else (cons (first lox) (all-but-last (rest lox)))]))

;;; render-segments : List-of-Posns Image Image -> Image
;;;
;;; Place img at each grid position in segs onto the given scene.
;;;
(check-expect (render-segments '() SEG-IMG1 BG) BG)
(check-expect (render-segments (list (make-posn 0 0)) SEG-IMG1 BG)
              (place-image SEG-IMG1 (grid->px 0) (grid->px 0) BG))
(check-expect (render-segments (list (make-posn 1 0) (make-posn 0 0)) SEG-IMG2 BG)
              (place-image SEG-IMG2 (grid->px 1) (grid->px 0)
                (place-image SEG-IMG2 (grid->px 0) (grid->px 0) BG)))

(define (render-segments segs img scene)
  (cond
    [(empty? segs) scene]
    [else (place-image img
            (grid->px (posn-x (first segs)))
            (grid->px (posn-y (first segs)))
            (render-segments (rest segs) img scene))]))

;;; food-create : Posn -> Posn
;;;
;;; Produce a random grid position different from the avoided position p.
;;; Uses generative recursion.
;;;
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)

(define (food-create p)
  (food-check-create
   p (make-posn (random MAX) (random MAX))))

;;; food-check-create : Posn Posn -> Posn
;;;
;;; If candidate equals p, try again; otherwise return candidate.
;;;
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

;;; not=-1-1? : Posn -> Boolean
;;;
;;; Produce #true iff the given Posn is NOT (1, 1).
;;; Used only for testing food-create.
;;;
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

;;;;
;;;; New food-list helpers
;;;;

;;; render-food : List-of-Posn Image -> Image
;;;
;;; Place FOOD-IMG at each food position in lof onto scene.
;;; Structurally identical to render-segments; factored out so
;;; game-render does not need a special case for the food list.
;;;
;;; lof   : a List-of-Posn (the food pellet positions)
;;; scene : an Image onto which pellets are drawn
;;;
;; CHANGED: new function (was a single place-image call in game-render)
(check-expect (render-food '() BG) BG)
(check-expect (render-food (list (make-posn 0 0)) BG)
              (place-image FOOD-IMG (grid->px 0) (grid->px 0) BG))
(check-expect (render-food (list (make-posn 1 0) (make-posn 0 0)) BG)
              (place-image FOOD-IMG (grid->px 1) (grid->px 0)
                (place-image FOOD-IMG (grid->px 0) (grid->px 0) BG)))

(define (render-food lof scene)
  (cond
    [(empty? lof) scene]
    [else (place-image FOOD-IMG
            (grid->px (posn-x (first lof)))
            (grid->px (posn-y (first lof)))
            (render-food (rest lof) scene))]))

;;; food-remove : Posn List-of-Posn -> List-of-Posn
;;;
;;; Remove the first occurrence of p from lof.  Used when a worm
;;; eats one pellet: the eaten pellet is dropped and a fresh one
;;; is added by food-create.
;;;
;;; p   : the Posn to remove (the eaten pellet)
;;; lof : the current food list
;;;
;; CHANGED: new function
(check-expect (food-remove (make-posn 1 1)
                           (list (make-posn 1 1) (make-posn 2 2)))
              (list (make-posn 2 2)))
(check-expect (food-remove (make-posn 2 2)
                           (list (make-posn 1 1) (make-posn 2 2) (make-posn 3 3)))
              (list (make-posn 1 1) (make-posn 3 3)))
(check-expect (food-remove (make-posn 9 9)
                           (list (make-posn 1 1) (make-posn 2 2)))
              (list (make-posn 1 1) (make-posn 2 2)))

(define (food-remove p lof)
  (cond
    [(empty? lof) '()]
    [(equal? p (first lof)) (rest lof)]
    [else (cons (first lof) (food-remove p (rest lof)))]))

;;; food-create-list : Number -> List-of-Posn
;;;
;;; Produce a list of n distinct random food pellets.  No two pellets
;;; in the result occupy the same grid position.
;;;
;;; Strategy: build the list one pellet at a time.  Each new pellet is
;;; checked against all previously placed ones; if it collides, a fresh
;;; candidate is tried (generative recursion via food-create-list-add).
;;; Termination is guaranteed as long as n <= GRID-W * GRID-H.
;;;
;;; n : a positive integer (<= GRID-W * GRID-H)
;;;
(define (food-create-list n)
  (cond
    [(zero? n) '()]
    [else
     (local [(define rest-food (food-create-list (sub1 n)))]
       (cons (food-create-list-add rest-food) rest-food))]))

;;; food-create-list-add : List-of-Posn -> Posn
;;;
;;; Generate a random grid position not already in existing.
;;; Uses generative recursion: each recursive call produces a fresh
;;; random candidate rather than decomposing the input.
;;;
;;; existing : the pellets already placed (all distinct)
;;;
(define (food-create-list-add existing)
  (local [(define candidate (make-posn (random MAX) (random MAX)))]
    (if (member? candidate existing)
        (food-create-list-add existing)
        candidate)))

;;;;
;;;; Worm helpers (carried forward UNCHANGED)
;;;;

;;; worm-move : Worm -> Worm
(check-expect (worm-move WORM1-1)
              (make-worm (list (make-posn 6 5)) "right"))
(check-expect (worm-move WORM1-3)
              (make-worm (list (make-posn 8 5) (make-posn 7 5) (make-posn 6 5))
                         "right"))

(define (worm-move w)
  (make-worm (cons (new-head (first (worm-segs w)) (worm-dir w))
                   (all-but-last (worm-segs w)))
             (worm-dir w)))

;;; worm-grow : Worm -> Worm
(check-expect (worm-grow WORM1-1)
              (make-worm (list (make-posn 6 5) (make-posn 5 5)) "right"))
(check-expect (worm-grow WORM1-3)
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
(check-expect (worm-steer WORM1-3 "up")
              (make-worm (worm-segs WORM1-3) "up"))
(check-expect (worm-steer WORM1-3 "left")
              (make-worm (worm-segs WORM1-3) "left"))
(check-expect (worm-steer WORM1-1 "right")
              (make-worm (worm-segs WORM1-1) "right"))
(check-expect (worm-steer WORM1-3 "a") WORM1-3)

(define (worm-steer w ke)
  (cond
    [(string=? ke "up")    (make-worm (worm-segs w) "up")]
    [(string=? ke "down")  (make-worm (worm-segs w) "down")]
    [(string=? ke "left")  (make-worm (worm-segs w) "left")]
    [(string=? ke "right") (make-worm (worm-segs w) "right")]
    [else w]))

;;; wasd->direction : KeyEvent -> KeyEvent
(check-expect (wasd->direction "w") "up")
(check-expect (wasd->direction "a") "left")
(check-expect (wasd->direction "s") "down")
(check-expect (wasd->direction "d") "right")
(check-expect (wasd->direction "x") "x")
(check-expect (wasd->direction "up") "up")

(define (wasd->direction ke)
  (cond
    [(string=? ke "w") "up"]
    [(string=? ke "s") "down"]
    [(string=? ke "a") "left"]
    [(string=? ke "d") "right"]
    [else ke]))

;;; worm-hit-wall? : Worm -> Boolean
(check-expect (worm-hit-wall? WORM1-3) #false)
(check-expect (worm-hit-wall? (make-worm (list (make-posn -1 5)) "left"))  #true)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 5 -1)) "up"))    #true)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 30 5)) "right")) #true)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 5 30)) "down"))  #true)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 0 0))  "up"))    #false)
(check-expect (worm-hit-wall? (make-worm (list (make-posn 29 29)) "down")) #false)

(define (worm-hit-wall? w)
  (or (< (posn-x (first (worm-segs w))) 0)
      (>= (posn-x (first (worm-segs w))) GRID-W)
      (< (posn-y (first (worm-segs w))) 0)
      (>= (posn-y (first (worm-segs w))) GRID-H)))

;;; worm-hit-self? : Worm -> Boolean
(check-expect (worm-hit-self? WORM1-1) #false)
(check-expect (worm-hit-self? WORM1-3) #false)
(check-expect (worm-hit-self?
               (make-worm (list (make-posn 5 5) (make-posn 6 5) (make-posn 5 5))
                          "left"))
              #true)

(define (worm-hit-self? w)
  (member? (first (worm-segs w)) (rest (worm-segs w))))

;;; worm-hit-other? : Worm Worm -> Boolean
(check-expect (worm-hit-other? WORM1-1 WORM2-1) #false)
(check-expect (worm-hit-other?
               (make-worm (list (make-posn 20 20)) "left")
               (make-worm (list (make-posn 20 20) (make-posn 21 20)) "left"))
              #true)
(check-expect (worm-hit-other?
               (make-worm (list (make-posn 5 5)) "right")
               (make-worm (list (make-posn 10 10) (make-posn 11 10)) "right"))
              #false)

(define (worm-hit-other? w other)
  (member? (first (worm-segs w)) (worm-segs other)))

;;; worm-eating? : Worm List-of-Posn -> Boolean
;;;
;;; Is the worm's next head position equal to any pellet in lof?
;;;
;; CHANGED: was (Worm Posn -> Boolean); now (Worm List-of-Posn -> Boolean)
;;          Signature change only; still a plain Boolean, ISL+-safe.
(check-expect (worm-eating? (make-worm (list (make-posn 5 5)) "right")
                            (list (make-posn 6 5) (make-posn 10 10)))
              #true)
(check-expect (worm-eating? (make-worm (list (make-posn 5 5)) "right")
                            (list (make-posn 10 10) (make-posn 20 20)))
              #false)
(check-expect (worm-eating? (make-worm (list (make-posn 5 5)) "up")
                            (list (make-posn 0 0) (make-posn 5 4)))
              #true)

(define (worm-eating? w lof)
  (local [(define next (new-head (first (worm-segs w)) (worm-dir w)))]
    ;; Returns #true if any pellet is hit; otherwise #false
    (ormap (lambda (p) (equal? next p)) lof)))

;;; worm-find-eaten : Worm List-of-Posn -> Posn
;;;
;;; Return the pellet in lof that the worm's next head lands on.
;;; Precondition: (worm-eating? w lof) is #true.
;;; Used by game-tock to identify which pellet to remove and replace.
;;;
;; CHANGED: new function, needed because ISL+ cond requires strict Booleans
;;          so worm-eating? cannot also return the eaten Posn.
(check-expect (worm-find-eaten (make-worm (list (make-posn 5 5)) "right")
                               (list (make-posn 6 5) (make-posn 10 10)))
              (make-posn 6 5))
(check-expect (worm-find-eaten (make-worm (list (make-posn 5 5)) "up")
                               (list (make-posn 0 0) (make-posn 5 4)))
              (make-posn 5 4))

(define (worm-find-eaten w lof)
  (local [(define next (new-head (first (worm-segs w)) (worm-dir w)))]
    (first (filter (lambda (p) (equal? next p)) lof))))

;;;;
;;;; Definitions
;;;;

;;; game-render : Game -> Image
;;;
;;; Produce an image of the game: food first (bottom layer), then worm 2
;;; (blue/turquoise), then worm 1 (red/pink) on top.  Each worm's head
;;; is drawn in its highlight colour; body segments use the base colour.
;;;
;; CHANGED: render-worm-with-head replaces render-worm; draws head separately.
(check-expect
 (game-render GAME-1)
 (place-image HEAD-IMG1
              (grid->px (posn-x (first (worm-segs WORM1-1))))
              (grid->px (posn-y (first (worm-segs WORM1-1))))
   (render-segments (rest (worm-segs WORM1-1)) SEG-IMG1
     (place-image HEAD-IMG2
                  (grid->px (posn-x (first (worm-segs WORM2-1))))
                  (grid->px (posn-y (first (worm-segs WORM2-1))))
       (render-segments (rest (worm-segs WORM2-1)) SEG-IMG2
         (render-food FOOD-5 BG))))))

(define (game-render g)
  (local
    [(define (render-worm-with-head w head-img body-img scene)
         (place-image head-img
           (grid->px (posn-x (first (worm-segs w))))
           (grid->px (posn-y (first (worm-segs w))))
           (render-segments (rest (worm-segs w)) body-img scene)))]
    (render-worm-with-head (game-worm1 g) HEAD-IMG1 SEG-IMG1
      (render-worm-with-head (game-worm2 g) HEAD-IMG2 SEG-IMG2
        (render-food (game-food g) BG)))))

;;; game-tock : Game -> Game
;;;
;;; Advance the game by one tick.  Each worm moves independently.
;;; Worm 1 is checked for eating first.
;;;
;;; Cases:
;;;   worm 1 eats pellet p: grow w1, move w2, replace p with a new pellet.
;;;   worm 2 eats pellet p: move w1, grow w2, replace p with a new pellet.
;;;   neither eats: move both, keep food list unchanged.
;;;
;; CHANGED: eating branches now use food-remove + food-create to replace
;;          the specific eaten pellet rather than replacing the single food Posn.

;; Normal tick, no eating
(check-expect (game-tock (make-game (make-worm (list (make-posn 5 5)) "right")
                                    (make-worm (list (make-posn 20 20)) "left")
                                    (list (make-posn 15 15))))
              (make-game (make-worm (list (make-posn 6 5)) "right")
                         (make-worm (list (make-posn 19 20)) "left")
                         (list (make-posn 15 15))))

;; Worm 1 eats the pellet at (6 5)
(check-random
 (game-tock (make-game (make-worm (list (make-posn 5 5)) "right")
                       (make-worm (list (make-posn 20 20)) "left")
                       (list (make-posn 6 5) (make-posn 15 15))))
 (make-game (worm-grow (make-worm (list (make-posn 5 5)) "right"))
            (worm-move (make-worm (list (make-posn 20 20)) "left"))
            (cons (food-create (make-posn 6 5))
                  (food-remove (make-posn 6 5)
                               (list (make-posn 6 5) (make-posn 15 15))))))

;; Worm 2 eats the pellet at (19 20)
(check-random
 (game-tock (make-game (make-worm (list (make-posn 5 5)) "right")
                       (make-worm (list (make-posn 20 20)) "left")
                       (list (make-posn 15 15) (make-posn 19 20))))
 (make-game (worm-move (make-worm (list (make-posn 5 5)) "right"))
            (worm-grow (make-worm (list (make-posn 20 20)) "left"))
            (cons (food-create (make-posn 19 20))
                  (food-remove (make-posn 19 20)
                               (list (make-posn 15 15) (make-posn 19 20))))))

(define (game-tock g)
  (local [(define w1   (game-worm1 g))
          (define w2   (game-worm2 g))
          (define food (game-food g))]
    (cond
      [(worm-eating? w1 food)
       (local [(define eaten (worm-find-eaten w1 food))]
         (make-game (worm-grow w1)
                    (worm-move w2)
                    (cons (food-create eaten)
                          (food-remove eaten food))))]
      [(worm-eating? w2 food)
       (local [(define eaten (worm-find-eaten w2 food))]
         (make-game (worm-move w1)
                    (worm-grow w2)
                    (cons (food-create eaten)
                          (food-remove eaten food))))]
      [else
       (make-game (worm-move w1)
                  (worm-move w2)
                  food)])))

;;; game-steer : Game KeyEvent -> Game  [UNCHANGED]
(check-expect (game-steer GAME-3 "up")
              (make-game (make-worm (worm-segs WORM1-3) "up")
                         WORM2-3
                         FOOD-5))
(check-expect (game-steer GAME-3 "w")
              (make-game WORM1-3
                         (make-worm (worm-segs WORM2-3) "up")
                         FOOD-5))
(check-expect (game-steer GAME-3 "a")
              (make-game WORM1-3
                         (make-worm (worm-segs WORM2-3) "left")
                         FOOD-5))
(check-expect (game-steer GAME-3 "s")
              (make-game WORM1-3
                         (make-worm (worm-segs WORM2-3) "down")
                         FOOD-5))
(check-expect (game-steer GAME-3 "d")
              (make-game WORM1-3
                         (make-worm (worm-segs WORM2-3) "right")
                         FOOD-5))
(check-expect (game-steer GAME-3 "q") GAME-3)

(define (game-steer g ke)
  (local [(define arrow? (lambda (k)
                           (or (string=? k "up")   (string=? k "down")
                               (string=? k "left") (string=? k "right"))))
          (define wasd?  (lambda (k)
                           (or (string=? k "w") (string=? k "a")
                               (string=? k "s") (string=? k "d"))))]
    (cond
      [(arrow? ke)
       (make-game (worm-steer (game-worm1 g) ke)
                  (game-worm2 g)
                  (game-food g))]
      [(wasd? ke)
       (make-game (game-worm1 g)
                  (worm-steer (game-worm2 g) (wasd->direction ke))
                  (game-food g))]
      [else g])))

;;; game-over? : Game -> Boolean  [UNCHANGED]
(check-expect (game-over? GAME-3) #false)
(check-expect (game-over?
               (make-game (make-worm (list (make-posn -1 5)) "left")
                          WORM2-1
                          FOOD-5))
              #true)
(check-expect (game-over?
               (make-game WORM1-1
                          (make-worm (list (make-posn 5 -1)) "up")
                          FOOD-5))
              #true)
(check-expect (game-over?
               (make-game
                (make-worm (list (make-posn 20 20)) "left")
                (make-worm (list (make-posn 20 20) (make-posn 21 20)) "right")
                FOOD-5))
              #true)

(define (game-over? g)
  (local [(define w1 (game-worm1 g))
          (define w2 (game-worm2 g))]
    (or (worm-hit-wall? w1) (worm-hit-self? w1) (worm-hit-other? w1 w2)
        (worm-hit-wall? w2) (worm-hit-self? w2) (worm-hit-other? w2 w1))))

;;; game-over-message : Game -> String  [UNCHANGED]
(check-expect
 (game-over-message
  (make-game (make-worm (list (make-posn -1 5)) "left") WORM2-1 FOOD-5))
 "worm 1 hit wall")
(check-expect
 (game-over-message
  (make-game WORM1-1
             (make-worm (list (make-posn 30 5)) "right")
             FOOD-5))
 "worm 2 hit wall")
(check-expect
 (game-over-message
  (make-game
   (make-worm (list (make-posn 5 5) (make-posn 6 5) (make-posn 5 5)) "left")
   WORM2-1 FOOD-5))
 "worm 1 hit self")
(check-expect
 (game-over-message
  (make-game
   (make-worm (list (make-posn 20 20)) "left")
   (make-worm (list (make-posn 20 20) (make-posn 21 20)) "right")
   FOOD-5))
 "worm 1 hit worm 2")

(define (game-over-message g)
  (local [(define w1 (game-worm1 g))
          (define w2 (game-worm2 g))]
    (cond
      [(worm-hit-wall?  w1)     "worm 1 hit wall"]
      [(worm-hit-self?  w1)     "worm 1 hit self"]
      [(worm-hit-other? w1 w2)  "worm 1 hit worm 2"]
      [(worm-hit-wall?  w2)     "worm 2 hit wall"]
      [(worm-hit-self?  w2)     "worm 2 hit self"]
      [else                     "worm 2 hit worm 1"])))

;;; game-render-final : Game -> Image  [UNCHANGED]
(check-expect
 (game-render-final
  (make-game (make-worm (list (make-posn -1 5)) "left") WORM2-1 FOOD-5))
 (place-image/align
  (text "worm 1 hit wall" FONT-SIZE FONT-COLOR)
  1 (- BOARD-H 1) "left" "bottom"
  (game-render
   (make-game (make-worm (list (make-posn -1 5)) "left") WORM2-1 FOOD-5))))

(define (game-render-final g)
  (place-image/align
   (text (game-over-message g) FONT-SIZE FONT-COLOR)
   1 (- BOARD-H 1) "left" "bottom"
   (game-render g)))

;;;;
;;;; Main
;;;;

;;; game-main : Worm Worm Number Boolean -> Number  [UNCHANGED except initial food]
;;;
;;; Launch the two-player worm game.  Returns the total segment count
;;; of both worms when the game ends.
;;;
;;; Example:
;;;   (game-main WORM1-3 WORM2-3 1/5 #false)
;;;
;; CHANGED: initial food is now (food-create-list NUM-FOOD) instead of
;;          (food-create (first (worm-segs worm1)))
(define (game-main worm1 worm2 rate show-state?)
  (local [(define final-game
            (big-bang (make-game worm1
                                 worm2
                                 (food-create-list NUM-FOOD))
              [on-tick   game-tock rate]
              [on-key    game-steer]
              [to-draw   game-render]
              [stop-when game-over? game-render-final]
              [state     show-state?]))]
    (+ (length (worm-segs (game-worm1 final-game)))
       (length (worm-segs (game-worm2 final-game))))))

;; To play:
(game-main WORM1-3 WORM2-3 1/5 #false)
;;
;; Player 1 (red):  arrow keys
;; Player 2 (blue): W A S D
