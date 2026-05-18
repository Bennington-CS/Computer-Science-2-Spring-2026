;;;;;
;;;;; Worm-Plus: Two-Player Worm Game (Exercise 219, extended)
;;;;; A world program
;;;;;

(require 2htdp/image)
(require 2htdp/universe)

#|
Two-player variant of the worm game from exercise 219.

Player 1 (red worm)  steers with the arrow keys.
Player 2 (blue worm) steers with W/A/S/D.

There is still exactly one piece of food on the board.  Either worm may
eat it.  When a worm eats, it grows by one segment and new food appears
elsewhere.

The game ends when either worm hits a wall, hits itself, or hits the
body of the other worm.

--- Design changes from the single-worm version ---

Data:
  Game gains a second worm field:
    (make-game worm1 worm2 food)

Rendering:
  render-segments now takes an image argument so each worm can be
  drawn in its own colour.  We pass SEG-IMG1 for the red worm and
  SEG-IMG2 for the blue worm.  This is the one place where ISL+
  lambda is used: render-worm is defined as
    (lambda (w img) (render-segments (worm-segs w) img scene))
  to avoid repeating the place-image logic twice inside game-render.

Steering:
  game-steer dispatches arrow keys to worm 1 and W/A/S/D to worm 2.
  All other keys are ignored.

Tick:
  Each worm moves (or grows) independently every tick.  The food is
  shared: whichever worm is about to eat it grows, and new food is
  placed avoiding that worm's new head.  If both worms are about to
  eat simultaneously (unlikely but possible), worm 1 takes priority.

Game over:
  worm-hit-other? checks whether a worm's head is inside the other
  worm's segment list.  The stop condition is:
    either worm hits a wall
    or either worm hits itself
    or either worm hits the other worm.

End screen:
  game-render-final reports which condition ended the game, prefixed
  by "worm 1:" or "worm 2:" when the cause is asymmetric.
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

;;; Graphical constants

(define SEG-IMG1 (circle (/ SEG-SIZE 2) "solid" "red"))   ; player 1 colour
(define SEG-IMG2 (circle (/ SEG-SIZE 2) "solid" "blue"))  ; player 2 colour
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
;;;   (make-game Worm Worm Posn)
;;; interpretation: (make-game w1 w2 f) is a game state where w1 is the
;;;   red (player-1) worm, w2 is the blue (player-2) worm, and f is the
;;;   grid position of the single piece of food.
;;;
;;; Examples:
;;;   (make-game (make-worm (list (make-posn 5 5)) "right")
;;;              (make-worm (list (make-posn 20 20)) "left")
;;;              (make-posn 10 10))
;;;     -- one-segment worms on opposite sides, food in the middle.

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

(define GAME-1 (make-game WORM1-1 WORM2-1 (make-posn 10 10)))
(define GAME-3 (make-game WORM1-3 WORM2-3 (make-posn 15 15)))

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
;;; The extra Image argument (compared to the single-worm version)
;;; lets callers choose the segment colour per worm.  Consumes a
;;; list of Posns, a segment image, and a scene; produces an Image.
;;;
;;; (define (render-segments segs img scene)
;;;   (cond
;;;     [(empty? segs) scene]
;;;     [else (place-image img
;;;             (grid->px (posn-x (first segs)))
;;;             (grid->px (posn-y (first segs)))
;;;             (render-segments (rest segs) img scene))]))
;;;
;;; segs  : a List-of-Posns (may be empty during recursion)
;;; img   : an Image to place at each position
;;; scene : an Image onto which segments are drawn
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
;;;; Worm helpers (carried forward unchanged)
;;;;

;;; worm-move : Worm -> Worm
;;;
;;; Move the worm one step forward (new head, drop last segment).
;;;
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
;;;
;;; Grow the worm by one segment (new head, keep all existing segments).
;;;
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
;;;
;;; Change the worm's direction in response to a direction key.
;;; All other keys are ignored.
;;;
;;; The caller is responsible for passing only the keys that belong
;;; to this worm.  worm-steer itself only understands "up", "down",
;;; "left", "right"; the mapping from WASD to those strings happens
;;; in game-steer before this function is called.
;;;
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
;;;
;;; Translate a W/A/S/D key into the corresponding direction string
;;; understood by worm-steer.  Any other key is passed through
;;; unchanged so that worm-steer's catch-all else clause ignores it.
;;;
;;; (define (wasd->direction ke)
;;;   (cond
;;;     [(string=? ke "w") "up"]
;;;     [(string=? ke "s") "down"]
;;;     [(string=? ke "a") "left"]
;;;     [(string=? ke "d") "right"]
;;;     [else ke]))
;;;
;;; ke : a KeyEvent (a String)
;;;
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
;;;
;;; Has the worm's head moved outside the grid?
;;;
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
;;;
;;; Has the worm's head landed on one of its own tail segments?
;;;
(check-expect (worm-hit-self? WORM1-1) #false)
(check-expect (worm-hit-self? WORM1-3) #false)
(check-expect (worm-hit-self?
               (make-worm (list (make-posn 5 5) (make-posn 6 5) (make-posn 5 5))
                          "left"))
              #true)

(define (worm-hit-self? w)
  (member? (first (worm-segs w)) (rest (worm-segs w))))

;;; worm-hit-other? : Worm Worm -> Boolean
;;;
;;; Has the first worm's head landed on any segment of the second worm?
;;; Consumes two Worms; produces a Boolean.
;;;
;;; (define (worm-hit-other? w other)
;;;   (member? (first (worm-segs w)) (worm-segs other)))
;;;
;;; w     : the worm whose head we are checking
;;; other : the worm whose body acts as the obstacle
;;;
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

;;; worm-eating? : Worm Posn -> Boolean
;;;
;;; Is the worm about to eat the food?  True when the next head
;;; position equals the food position.
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
;;; Produce an image of the game: food first (bottom layer), then worm 2
;;; (blue), then worm 1 (red) on top.  Lambda is used here to factor out
;;; the repeated "render this worm's segments onto the current scene"
;;; step without defining a separate top-level helper.
;;;
;;; The local render-worm function has signature:
;;;   Worm Image Image -> Image
;;; It places all segments of w using colour img onto scene.
;;;
;;; g : a Game
;;;
(check-expect
 (game-render GAME-1)
 (render-segments (worm-segs WORM1-1) SEG-IMG1
   (render-segments (worm-segs WORM2-1) SEG-IMG2
     (place-image FOOD-IMG (grid->px 10) (grid->px 10) BG))))

(define (game-render g)
  (local
    [(define render-worm
       (lambda (w img scene)
         (render-segments (worm-segs w) img scene)))]
    (render-worm (game-worm1 g) SEG-IMG1
      (render-worm (game-worm2 g) SEG-IMG2
        (place-image FOOD-IMG
          (grid->px (posn-x (game-food g)))
          (grid->px (posn-y (game-food g)))
          BG)))))

;;; game-tock : Game -> Game
;;;
;;; Advance the game by one tick.  Each worm moves independently.
;;; Worm 1 is checked for eating first; if it eats, worm 2 still
;;; moves normally on the same tick (the food is gone after worm 1
;;; eats it, so worm 2 cannot also eat this tick).
;;;
;;; Cases:
;;;   worm 1 eats: grow w1, move w2, place new food avoiding w1's new head.
;;;   worm 2 eats: move w1, grow w2, place new food avoiding w2's new head.
;;;   neither eats: move both, keep food.
;;;
;;; g : a Game
;;;

;; Normal tick, no eating
(check-expect (game-tock (make-game (make-worm (list (make-posn 5 5)) "right")
                                    (make-worm (list (make-posn 20 20)) "left")
                                    (make-posn 15 15)))
              (make-game (make-worm (list (make-posn 6 5)) "right")
                         (make-worm (list (make-posn 19 20)) "left")
                         (make-posn 15 15)))

;; Worm 1 eats
(check-random
 (game-tock (make-game (make-worm (list (make-posn 5 5)) "right")
                       (make-worm (list (make-posn 20 20)) "left")
                       (make-posn 6 5)))
 (make-game (worm-grow (make-worm (list (make-posn 5 5)) "right"))
            (worm-move (make-worm (list (make-posn 20 20)) "left"))
            (food-create (make-posn 6 5))))

;; Worm 2 eats
(check-random
 (game-tock (make-game (make-worm (list (make-posn 5 5)) "right")
                       (make-worm (list (make-posn 20 20)) "left")
                       (make-posn 19 20)))
 (make-game (worm-move (make-worm (list (make-posn 5 5)) "right"))
            (worm-grow (make-worm (list (make-posn 20 20)) "left"))
            (food-create (make-posn 19 20))))

(define (game-tock g)
  (local [(define w1   (game-worm1 g))
          (define w2   (game-worm2 g))
          (define food (game-food g))]
    (cond
      [(worm-eating? w1 food)
       (make-game (worm-grow w1)
                  (worm-move w2)
                  (food-create
                   (first (worm-segs (worm-grow w1)))))]
      [(worm-eating? w2 food)
       (make-game (worm-move w1)
                  (worm-grow w2)
                  (food-create
                   (first (worm-segs (worm-grow w2)))))]
      [else
       (make-game (worm-move w1)
                  (worm-move w2)
                  food)])))

;;; game-steer : Game KeyEvent -> Game
;;;
;;; Route keystrokes to the appropriate worm.
;;;   Arrow keys        -> steer worm 1 (player 1).
;;;   W / A / S / D     -> translate to directions, then steer worm 2.
;;;   Everything else   -> ignored (both worms unchanged).
;;;
;;; (define (game-steer g ke)
;;;   (cond
;;;     [(arrow-key? ke)
;;;      (make-game (worm-steer (game-worm1 g) ke)
;;;                 (game-worm2 g) (game-food g))]
;;;     [(wasd-key? ke)
;;;      (make-game (game-worm1 g)
;;;                 (worm-steer (game-worm2 g) (wasd->direction ke))
;;;                 (game-food g))]
;;;     [else g]))
;;;
;;; g  : a Game
;;; ke : a KeyEvent
;;;
(check-expect (game-steer GAME-3 "up")
              (make-game (make-worm (worm-segs WORM1-3) "up")
                         WORM2-3
                         (make-posn 15 15)))
(check-expect (game-steer GAME-3 "w")
              (make-game WORM1-3
                         (make-worm (worm-segs WORM2-3) "up")
                         (make-posn 15 15)))
(check-expect (game-steer GAME-3 "a")
              (make-game WORM1-3
                         (make-worm (worm-segs WORM2-3) "left")
                         (make-posn 15 15)))
(check-expect (game-steer GAME-3 "s")
              (make-game WORM1-3
                         (make-worm (worm-segs WORM2-3) "down")
                         (make-posn 15 15)))
(check-expect (game-steer GAME-3 "d")
              (make-game WORM1-3
                         (make-worm (worm-segs WORM2-3) "right")
                         (make-posn 15 15)))
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

;;; game-over? : Game -> Boolean
;;;
;;; Should the game stop?  True when any worm has hit a wall, hit
;;; itself, or hit the body of the other worm.
;;;
;;; (define (game-over? g)
;;;   (local [(define w1 (game-worm1 g))
;;;           (define w2 (game-worm2 g))]
;;;     (or (worm-hit-wall? w1) (worm-hit-self? w1) (worm-hit-other? w1 w2)
;;;         (worm-hit-wall? w2) (worm-hit-self? w2) (worm-hit-other? w2 w1))))
;;;
;;; g : a Game
;;;
(check-expect (game-over? GAME-3) #false)
(check-expect (game-over?
               (make-game (make-worm (list (make-posn -1 5)) "left")
                          WORM2-1
                          (make-posn 10 10)))
              #true)
(check-expect (game-over?
               (make-game WORM1-1
                          (make-worm (list (make-posn 5 -1)) "up")
                          (make-posn 10 10)))
              #true)
(check-expect (game-over?
               (make-game
                (make-worm (list (make-posn 20 20)) "left")
                (make-worm (list (make-posn 20 20) (make-posn 21 20)) "right")
                (make-posn 10 10)))
              #true)

(define (game-over? g)
  (local [(define w1 (game-worm1 g))
          (define w2 (game-worm2 g))]
    (or (worm-hit-wall? w1) (worm-hit-self? w1) (worm-hit-other? w1 w2)
        (worm-hit-wall? w2) (worm-hit-self? w2) (worm-hit-other? w2 w1))))

;;; game-over-message : Game -> String
;;;
;;; Produce a short message explaining why the game ended.
;;; Used by game-render-final.  Checks worm 1's conditions first.
;;; If multiple conditions are true simultaneously, the first matching
;;; clause wins (priority: w1 wall, w1 self, w1 hits w2, w2 wall,
;;; w2 self, w2 hits w1).
;;;
;;; g : a Game
;;;
(check-expect
 (game-over-message
  (make-game (make-worm (list (make-posn -1 5)) "left") WORM2-1 (make-posn 10 10)))
 "worm 1 hit wall")
(check-expect
 (game-over-message
  (make-game WORM1-1
             (make-worm (list (make-posn 30 5)) "right")
             (make-posn 10 10)))
 "worm 2 hit wall")
(check-expect
 (game-over-message
  (make-game
   (make-worm (list (make-posn 5 5) (make-posn 6 5) (make-posn 5 5)) "left")
   WORM2-1 (make-posn 10 10)))
 "worm 1 hit self")
(check-expect
 (game-over-message
  (make-game
   (make-worm (list (make-posn 20 20)) "left")
   (make-worm (list (make-posn 20 20) (make-posn 21 20)) "right")
   (make-posn 10 10)))
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

;;; game-render-final : Game -> Image
;;;
;;; Produce the final scene: overlay the game-over message in the lower
;;; left corner of the normal game rendering.
;;;
;;; g : a Game
;;;
(check-expect
 (game-render-final
  (make-game (make-worm (list (make-posn -1 5)) "left") WORM2-1 (make-posn 10 10)))
 (place-image/align
  (text "worm 1 hit wall" FONT-SIZE FONT-COLOR)
  1 (- BOARD-H 1) "left" "bottom"
  (game-render
   (make-game (make-worm (list (make-posn -1 5)) "left") WORM2-1 (make-posn 10 10)))))

(define (game-render-final g)
  (place-image/align
   (text (game-over-message g) FONT-SIZE FONT-COLOR)
   1 (- BOARD-H 1) "left" "bottom"
   (game-render g)))

;;;;
;;;; Main
;;;;

;;; game-main : Worm Worm Number Boolean -> Number
;;;
;;; Launch the two-player worm game.  The first two arguments are the
;;; initial worm states for players 1 and 2.  rate is the clock tick
;;; rate in seconds.  show-state? controls the big-bang state inspector.
;;; Returns the total number of segments across both worms when the game
;;; ends (a rough combined score).
;;;
;;; Example:
;;;   (game-main WORM1-3 WORM2-3 1/5 #false)
;;;
(define (game-main worm1 worm2 rate show-state?)
  (local [(define final-game
            (big-bang (make-game worm1
                                 worm2
                                 (food-create (first (worm-segs worm1))))
              [on-tick   game-tock rate]
              [on-key    game-steer]
              [to-draw   game-render]
              [stop-when game-over? game-render-final]
              [state     show-state?]))]
    (+ (length (worm-segs (game-worm1 final-game)))
       (length (worm-segs (game-worm2 final-game))))))

;; To play:
;; (game-main WORM1-3 WORM2-3 1/5 #false)
;;
;; Player 1 (red):  arrow keys
;; Player 2 (blue): W A S D
