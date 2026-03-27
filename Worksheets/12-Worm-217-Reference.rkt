;;;;;
;;;;; Multi-Segment Worm
;;;;; A world program
;;;;;

(require 2htdp/image)
(require 2htdp/universe)

#|
Exercise 217. Develop a data representation for worms with tails. A worm's
tail is a possibly empty sequence of "connected" segments. Here "connected"
means that the coordinates of a segment differ from those of its predecessor
in at most one direction. To keep things simple, treat all segments -- head
and tail segments -- the same.

Now modify your program from exercise 215 to accommodate a multi-segment
worm. Keep things simple: (1) your program may render all worm segments as
red disks and (2) ignore that the worm may run into the wall or itself.
Hint One way to realize the worm's movement is to add a segment in the
direction in which it is moving and to delete the last one.
|#

;;;; To code:
;;;; physical and graphical constants
;;;; worm struct (segs, dir) where segs is a non-empty list of Posns
;;;;
;;;; Wishes:
;;;; grid->px          -- convert grid coordinate to pixel center
;;;; new-head          -- compute the next head position given current head and direction
;;;; all-but-last      -- drop the last element of a non-empty list
;;;; render-segments   -- place every segment image onto a scene
;;;;
;;;; Definitions:
;;;; worm-render  (to-draw)
;;;; worm-move    (on-tick)
;;;; worm-steer   (on-key)
;;;; worm-main    (main)

;;;;
;;;; Constants
;;;;

;;; Physical constants

(define SEG-SIZE 10)                  ; diameter of one worm segment (in pixels)
(define GRID-W  30)                  ; width of the game board (in segments)
(define GRID-H  30)                  ; height of the game board (in segments)
(define BOARD-W (* GRID-W SEG-SIZE)) ; width of the game board (in pixels)
(define BOARD-H (* GRID-H SEG-SIZE)) ; height of the game board (in pixels)

;;; Graphical constants

(define SEG-IMG (circle (/ SEG-SIZE 2) "solid" "red"))
(define BG      (empty-scene BOARD-W BOARD-H))

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
;;; interpretation: a non-empty list of grid positions, where the first
;;;   element is the head of the worm and the remaining elements are the
;;;   tail segments from front to back.
;;;
;;; "Connected" means each segment differs from its predecessor by at most
;;; one unit in one direction.
;;;
;;; Examples:
;;;   (list (make-posn 5 5))
;;;     -- a one-segment worm at grid position (5, 5)
;;;   (list (make-posn 7 5) (make-posn 6 5) (make-posn 5 5))
;;;     -- a three-segment worm with head at (7, 5) stretching left

(define-struct worm [segs dir])
;;; A Worm is a structure:
;;;   (make-worm NEList-of-Posns Direction)
;;; interpretation: (make-worm segs dir) represents a worm whose segments
;;;   are at the grid positions in segs (head first) and which currently
;;;   moves in direction dir.
;;;
;;; Examples:
;;;   (make-worm (list (make-posn 5 5)) "right")
;;;     -- one-segment worm at center, heading right
;;;   (make-worm (list (make-posn 7 5) (make-posn 6 5) (make-posn 5 5)) "right")
;;;     -- three-segment worm heading right

;;; Shorthand examples:

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

;;;;
;;;; Wishes (helper functions)
;;;;

;;; grid->px : Number -> Number
;;;
;;; Convert a logical grid coordinate to the pixel coordinate of the center
;;; of that grid cell.  Consumes a grid coordinate and produces a pixel
;;; coordinate.
;;;
;;; (define (grid->px g)
;;;   (... g ... SEG-SIZE ...))
;;;
;;; g : a non-negative integer, the grid coordinate
;;;
(check-expect (grid->px 0) (/ SEG-SIZE 2))
(check-expect (grid->px 1) (+ SEG-SIZE (/ SEG-SIZE 2)))
(check-expect (grid->px 5) (+ (* 5 SEG-SIZE) (/ SEG-SIZE 2)))

(define (grid->px g)
  (+ (* g SEG-SIZE) (/ SEG-SIZE 2)))

;;; new-head : Posn Direction -> Posn
;;;
;;; Compute the grid position one step from the given position in the given
;;; direction.  Consumes a Posn (the current head) and a Direction, and
;;; produces a Posn (the new head position).
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
;;; Consumes a non-empty list and produces a list.  When the input has
;;; exactly one element, the result is '().
;;;
;;; (define (all-but-last lox)
;;;   (cond
;;;     [(empty? (rest lox)) ...]
;;;     [else (cons (first lox) ... (all-but-last (rest lox)) ...)]))
;;;
;;; lox : a non-empty list
;;;
(check-expect (all-but-last (list "a"))
              '())
(check-expect (all-but-last (list "a" "b"))
              (list "a"))
(check-expect (all-but-last (list "a" "b" "c"))
              (list "a" "b"))

(define (all-but-last lox)
  (cond
    [(empty? (rest lox)) '()]
    [else (cons (first lox) (all-but-last (rest lox)))]))

;;; render-segments : NEList-of-Posns Image -> Image
;;;
;;; Place a SEG-IMG at each grid position in the list onto the given scene.
;;; Consumes a list of Posns and a scene, and produces an Image.
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
(check-expect (render-segments '() BG)
              BG)
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

;;;;
;;;; Definitions
;;;;

;;; worm-render : Worm -> Image
;;;
;;; Produce an image of the game board with every segment of the worm drawn
;;; at its grid position.  Consumes a Worm and produces an Image.
;;;
;;; (define (worm-render w)
;;;   (render-segments (worm-segs w) BG))
;;;
;;; w : a Worm
;;;
(check-expect (worm-render WORM-1)
              (render-segments (list (make-posn 5 5)) BG))
(check-expect (worm-render WORM-3)
              (render-segments (list (make-posn 7 5) (make-posn 6 5) (make-posn 5 5)) BG))

(define (worm-render w)
  (render-segments (worm-segs w) BG))

;;; worm-move : Worm -> Worm
;;;
;;; Move the worm one segment in its current direction.  This works by
;;; adding a new head segment in the direction of travel and dropping the
;;; last tail segment, so the worm appears to slither forward while
;;; keeping the same length.  Consumes a Worm and produces a Worm.
;;;
;;; (define (worm-move w)
;;;   (make-worm (cons (new-head ...) (all-but-last ...))
;;;              (worm-dir w)))
;;;
;;; w : a Worm
;;;
(check-expect (worm-move WORM-1)
              (make-worm (list (make-posn 6 5)) "right"))
(check-expect (worm-move WORM-3)
              (make-worm (list (make-posn 8 5) (make-posn 7 5) (make-posn 6 5)) "right"))
(check-expect (worm-move (make-worm (list (make-posn 5 5)) "up"))
              (make-worm (list (make-posn 5 4)) "up"))
(check-expect (worm-move (make-worm (list (make-posn 5 5) (make-posn 5 4)) "down"))
              (make-worm (list (make-posn 5 6) (make-posn 5 5)) "down"))

(define (worm-move w)
  (make-worm (cons (new-head (first (worm-segs w)) (worm-dir w))
                   (all-but-last (worm-segs w)))
             (worm-dir w)))

;;; worm-steer : Worm String -> Worm
;;;
;;; Change the worm's direction in response to an arrow key press.  Consumes
;;; a Worm and a KeyEvent and produces a Worm.  Arrow keys change the
;;; direction; all other keys are ignored.
;;;
;;; (define (worm-steer w ke)
;;;   (cond
;;;     [(string=? ke "up")    (make-worm (worm-segs w) "up")]
;;;     [(string=? ke "down")  (make-worm (worm-segs w) "down")]
;;;     [(string=? ke "left")  (make-worm (worm-segs w) "left")]
;;;     [(string=? ke "right") (make-worm (worm-segs w) "right")]
;;;     [else w]))
;;;
;;; w  : a Worm
;;; ke : a KeyEvent (a String)
;;;
(check-expect (worm-steer WORM-3 "up")
              (make-worm (worm-segs WORM-3) "up"))
(check-expect (worm-steer WORM-3 "down")
              (make-worm (worm-segs WORM-3) "down"))
(check-expect (worm-steer WORM-3 "left")
              (make-worm (worm-segs WORM-3) "left"))
(check-expect (worm-steer WORM-1 "right")
              (make-worm (worm-segs WORM-1) "right"))
(check-expect (worm-steer WORM-3 "a")
              WORM-3)
(check-expect (worm-steer WORM-3 " ")
              WORM-3)

(define (worm-steer w ke)
  (cond
    [(string=? ke "up")    (make-worm (worm-segs w) "up")]
    [(string=? ke "down")  (make-worm (worm-segs w) "down")]
    [(string=? ke "left")  (make-worm (worm-segs w) "left")]
    [(string=? ke "right") (make-worm (worm-segs w) "right")]
    [else w]))

;;;;
;;;; Main
;;;;

;;; worm-main : Number -> Worm
;;;
;;; Launch the worm game.  The argument is the clock tick rate in seconds
;;; (e.g., 0.5 means one tick every half second).  The worm starts near
;;; the center of the board as a three-segment worm heading right.
;;;
;;; Example:
;;;   (worm-main 1/3)
;;;
(define (worm-main worm rate)
  (big-bang worm
    [on-tick  worm-move rate]
    [on-key   worm-steer]
    [to-draw  worm-render]))

;(worm-main WORM-5 1/3)
