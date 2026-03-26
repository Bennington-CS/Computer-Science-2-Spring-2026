;;;;;
;;;;; Worm
;;;;; A world program
;;;;;

(require 2htdp/image)
(require 2htdp/universe)

;;;; The player controls a one-segment worm using the four arrow keys.
;;;; The worm moves one diameter per clock tick in whatever direction
;;;; it currently faces.

#|
Exercise 215. Design a world program that continually moves a one-segment
worm and enables a player to control the movement of the worm with the
four cardinal arrow keys. Your program should use a red disk to render the
one-and-only segment of the worm. For each clock tick, the worm should move
a diameter.

Hints (1) Reread Designing World Programs to recall how to design world 
programs. When you define the worm-main function, use the rate at which
the clock ticks as its argument. See the documentation for on-tick on how
to describe the rate. (2) When you develop a data representation for the
worm, contemplate the use of two different kinds of representations: a
physical representation and a logical one. The physical representation
keeps track of the actual physical position of the worm on the canvas; the
logical one counts how many (widths of) segments the worm is from the left
and the top. For which of the two is it easier to change the physical
appearances (size of worm segment, size of game box) of the “game”? 
|#

;;;; To code:
;;;; physical and graphical constants
;;;; worm struct (x, y, dir)
;;;;
;;;; Wishes:
;;;; grid->px
;;;;
;;;; Definitions:
;;;; worm-render (draw)
;;;; worm-move (on-tick)
;;;; worm-steer (on-key)
;;;; worm-main (to-draw)

;;;;
;;;; Constants
;;;;

;;; Physical constants

(define SEG-SIZE 10)                 ; diameter of one worm segment (in pixels)
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

;;; We use a logical coordinate system: the worm's position is measured in
;;; segment-widths from the left edge and the top edge, not in raw pixels.
;;; This makes it easy to change the physical appearance (segment size,
;;; board dimensions) by editing constants alone.

(define-struct worm [x y dir])
;;; A Worm is a structure:
;;;   (make-worm Number Number Direction)
;;; interpretation: (make-worm x y dir) represents a worm whose segment
;;;   is x segment-widths from the left edge, y segment-widths from the
;;;   top edge, and currently moving in direction dir.
;;;
;;; Examples:
;;;   (make-worm 5 5 "right") -- center-ish, heading right
;;;   (make-worm 0 0 "down")  -- top-left corner, heading down

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

;;;;
;;;; Definitions
;;;;

;;; worm-render : Worm -> Image
;;;
;;; Produce an image of the game board with the worm's segment drawn at its
;;; current logical position.  Consumes a Worm and produces an Image.
;;;
;;; (define (worm-render w)
;;;   (... (worm-x w) ... (worm-y w) ... SEG-IMG ... BG ...))
;;;
;;; w : a Worm
;;;
(check-expect (worm-render (make-worm 0 0 "right"))
              (place-image SEG-IMG (grid->px 0) (grid->px 0) BG))
(check-expect (worm-render (make-worm 5 3 "left"))
              (place-image SEG-IMG (grid->px 5) (grid->px 3) BG))

(define (worm-render w)
  (place-image SEG-IMG
               (grid->px (worm-x w))
               (grid->px (worm-y w))
               BG))

;;; worm-move : Worm -> Worm
;;;
;;; Move the worm one segment in its current direction.  Consumes a Worm and
;;; produces a new Worm whose position has advanced by one grid cell.
;;;
;;; (define (worm-move w)
;;;   (cond
;;;     [(string=? (worm-dir w) "up")    (make-worm ... ...)]
;;;     [(string=? (worm-dir w) "down")  (make-worm ... ...)]
;;;     [(string=? (worm-dir w) "left")  (make-worm ... ...)]
;;;     [(string=? (worm-dir w) "right") (make-worm ... ...)]))
;;;
;;; w : a Worm
;;;
(check-expect (worm-move (make-worm 5 5 "right")) (make-worm 6 5 "right"))
(check-expect (worm-move (make-worm 5 5 "left"))  (make-worm 4 5 "left"))
(check-expect (worm-move (make-worm 5 5 "up"))    (make-worm 5 4 "up"))
(check-expect (worm-move (make-worm 5 5 "down"))  (make-worm 5 6 "down"))

(define (worm-move w)
  (cond
    [(string=? (worm-dir w) "up") (make-worm (worm-x w) (sub1 (worm-y w)) "up")]
    [(string=? (worm-dir w) "down") (make-worm (worm-x w) (add1 (worm-y w)) "down")]
    [(string=? (worm-dir w) "left") (make-worm (sub1 (worm-x w)) (worm-y w) "left")]
    [(string=? (worm-dir w) "right") (make-worm (add1 (worm-x w)) (worm-y w) "right")]))

;;; worm-steer : Worm String -> Worm
;;;
;;; Change the worm's direction in response to an arrow key press.  Consumes
;;; a Worm and a KeyEvent and produces a Worm.  Arrow keys change the
;;; direction; all other keys are ignored.
;;;
;;; (define (worm-steer w ke)
;;;   (cond
;;;     [(string=? ke "up")    (make-worm ... ... "up")]
;;;     [(string=? ke "down")  (make-worm ... ... "down")]
;;;     [(string=? ke "left")  (make-worm ... ... "left")]
;;;     [(string=? ke "right") (make-worm ... ... "right")]
;;;     [else w]))
;;;
;;; w  : a Worm
;;; ke : a KeyEvent (a String)
;;;
(check-expect (worm-steer (make-worm 5 5 "right") "up")
              (make-worm 5 5 "up"))
(check-expect (worm-steer (make-worm 5 5 "right") "down")
              (make-worm 5 5 "down"))
(check-expect (worm-steer (make-worm 5 5 "right") "left")
              (make-worm 5 5 "left"))
(check-expect (worm-steer (make-worm 5 5 "up") "right")
              (make-worm 5 5 "right"))
(check-expect (worm-steer (make-worm 5 5 "right") "a")
              (make-worm 5 5 "right"))
(check-expect (worm-steer (make-worm 5 5 "right") " ")
              (make-worm 5 5 "right"))

(define (worm-steer w ke)
  (cond
    [(string=? ke "up")    (make-worm (worm-x w) (worm-y w) "up")]
    [(string=? ke "down")  (make-worm (worm-x w) (worm-y w) "down")]
    [(string=? ke "left")  (make-worm (worm-x w) (worm-y w) "left")]
    [(string=? ke "right") (make-worm (worm-x w) (worm-y w) "right")]
    [else w]))

;;;;
;;;; Main
;;;;

;;; worm-main : Number -> Worm
;;;
;;; Launch the worm game.  The argument is the clock tick rate in seconds
;;; (e.g., 0.5 means one tick every half second).  The worm starts near
;;; the center of the board, heading right.
;;;
;;; Example:
;;;   (worm-main 1/3)
;;;
(define (worm-main rate)
  (big-bang (make-worm (quotient GRID-W 2) (quotient GRID-H 2) "right")
    [on-tick  worm-move rate]
    [on-key   worm-steer]
    [to-draw  worm-render]))

#|
Exercise 216. Modify your program from exercise 215 so that it stops if the
worm has reached the walls of the world. When the program stops because of
this condition, it should render the final scene with the text "worm hit
border" in the lower left of the world scene. Hint You can use the stop-
when clause in big-bang to render the last world in a special way.
|#

;;;;
;;;; New constants
;;;;

(define GAME-OVER-TEXT (text "Worm hit border!" 14 "black"))
(define TEXT-X (/ (image-width GAME-OVER-TEXT) 2))
(define TEXT-Y (- BOARD-H (/ (image-height GAME-OVER-TEXT) 2)))

;;;;
;;;; Definitions
;;;;

;;; worm-hit-border? : Worm -> Boolean
;;;
;;; Has the worm moved outside the grid?  Consumes a Worm and produces #true
;;; if its x or y coordinate is outside the valid range [0, GRID-W) or
;;; [0, GRID-H).
;;;
;;; (define (worm-hit-border? w)
;;;   (... (worm-x w) ... (worm-y w) ... GRID-W ... GRID-H ...))
;;;
;;; w : a Worm
;;;
(check-expect (worm-hit-border? (make-worm 5 5 "right"))  #false)
(check-expect (worm-hit-border? (make-worm 0 0 "up"))     #false)
(check-expect (worm-hit-border? (make-worm 29 29 "down")) #false)
(check-expect (worm-hit-border? (make-worm -1 5 "left"))  #true)
(check-expect (worm-hit-border? (make-worm 5 -1 "up"))    #true)
(check-expect (worm-hit-border? (make-worm 30 5 "right")) #true)
(check-expect (worm-hit-border? (make-worm 5 30 "down"))  #true)

(define (worm-hit-border? w)
  (or (< (worm-x w) 0)
      (< (worm-y w) 0)
      (>= (worm-x w) GRID-W)
      (>= (worm-y w) GRID-H)))

;;; worm-render-final : Worm -> Image
;;;
;;; Render the final scene when the worm has hit a border.  Produces the
;;; normal game image with the text "worm hit border" placed in the lower
;;; left corner of the scene.
;;;
;;; (define (worm-render-final w)
;;;   (place-image ... (worm-render w)))
;;;
;;; w : a Worm
;;;
(check-expect (worm-render-final (make-worm -1 5 "left"))
              (place-image GAME-OVER-TEXT
                           TEXT-X TEXT-Y
                           (worm-render (make-worm -1 5 "left"))))

(define (worm-render-final w)
  (place-image GAME-OVER-TEXT
               TEXT-X
               TEXT-Y
               (worm-render w)))

;;;
;;; Modified worm-main: added stop-when clause
;;;
(define (worm-main-v2 rate)
  (big-bang (make-worm (quotient GRID-W 2) (quotient GRID-H 2) "right")
    [on-tick   worm-move rate]
    [on-key    worm-steer]
    [to-draw   worm-render]
    [stop-when worm-hit-border? worm-render-final]))

; (worm-main-v2 1/3)
