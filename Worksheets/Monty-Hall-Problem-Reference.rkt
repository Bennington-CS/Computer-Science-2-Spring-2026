#lang htdp/isl+
;;;;;
;;;;; Monty Hall Simulator
;;;;; Reference Implementation (ISL+)
;;;;;

;;;;
;;;; Problem Statement
;;;;

;;;
;;; Simulate the Monty Hall problem with num-doors doors.
;;;
;;; One trial:
;;;   1. The prize is placed behind a random door in [0, num-doors).
;;;   2. The contestant picks a random door in [0, num-doors).
;;;   3. Monty reveals a goat door that is neither the prize nor the pick.
;;;   4. We record two outcomes on the SAME random draws:
;;;      - the "stayer" keeps the original pick
;;;      - the "switcher" moves to an unrevealed, unpicked door
;;;
;;; Run num-trials trials and tally wins for each strategy.  The classic
;;; result for 3 doors is that switching wins about 2/3 of the time and
;;; staying wins about 1/3.
;;;

;;;;
;;;; Data Definitions
;;;;

(define-struct result [stay switch])
;;; A Result is a (make-result Boolean Boolean)
;;; interpretation:
;;; - stay   is #true if the stayer wins this trial
;;; - switch is #true if the switcher wins this trial

(define-struct totals [stay switch])
;;; A Totals is a (make-totals Natural Natural)
;;; interpretation:
;;; - stay   is the running count of stayer wins
;;; - switch is the running count of switcher wins

;;;;
;;;; Wishes (helper functions)
;;;;

;;; random-door : Natural -> Natural
;;;
;;; Pick a uniformly random door from 0 to num-doors - 1.  Trivial wrapper
;;; around random, but named for readability at the call sites.
;;;
;;; (define (random-door num-doors)
;;;   ... random ...)
;;;
;;; num-doors : the total number of doors in the game
;;;
(define (random-door num-doors)
  (random num-doors))

;;; random-door-except : Natural Natural -> Natural
;;;
;;; Pick a uniformly random door in [0, num-doors), excluding the
;;; forbidden door.  Used by the switcher to land on a new door when
;;; staying would already win (so we need to switch to any other door).
;;;
;;; (define (random-door-except num-doors forbidden)
;;;   ... random-door ... random-door-except ...)
;;;
;;; num-doors : the total number of doors in the game
;;; forbidden : a Natural in [0, num-doors) that the result must differ
;;;             from
;;;
;;; PRECONDITION: num-doors >= 2 (otherwise there is no valid door).
;;;
;;; This is a GENERATIVE RECURSION:
;;;
;;; - trivially solvable case: the random draw is not forbidden, in which
;;;   case we return it directly.
;;;
;;; - generation step: the random draw IS forbidden, so we generate a
;;;   new instance of the same problem (same num-doors, same forbidden)
;;;   and try again.  The new problem is not structurally smaller -- it
;;;   is the same problem -- but it uses a fresh random draw.
;;;
;;; - termination argument: termination is PROBABILISTIC.  Each draw
;;;   independently has probability 1/num-doors of being forbidden and
;;;   probability (num-doors - 1)/num-doors of succeeding.  The chance
;;;   of failing k times in a row is (1/num-doors)^k, which goes to 0
;;;   as k grows.  The expected number of draws is num-doors /
;;;   (num-doors - 1) -- for example, 1.5 draws when num-doors = 3.
;;;   The function terminates with probability 1, but does not have a
;;;   hard upper bound on the number of draws.
;;;
(define (random-door-except num-doors forbidden)
  (local [(define candidate (random-door num-doors))]
    (cond
      [(= candidate forbidden) (random-door-except num-doors forbidden)]
      [else candidate])))

(check-satisfied (random-door-except 3 0)
                 (lambda (n) (and (<= 0 n) (< n 3) (not (= n 0)))))
(check-satisfied (random-door-except 3 2)
                 (lambda (n) (and (<= 0 n) (< n 3) (not (= n 2)))))
(check-satisfied (random-door-except 5 3)
                 (lambda (n) (and (<= 0 n) (< n 5) (not (= n 3)))))

;;; count-win : Boolean -> Natural
;;;
;;; Convert a win/loss Boolean into 1 or 0 for tallying.
;;;
;;; (define (count-win b)
;;;   ... if ...)
;;;
;;; b : a Boolean indicating whether a strategy won
;;;
(define (count-win b)
  (if b 1 0))

(check-expect (count-win #true) 1)
(check-expect (count-win #false) 0)

;;;;
;;;; Definitions
;;;;

;;; play-once : Natural -> Result
;;;
;;; Play one Monty Hall trial with num-doors doors.  Both strategies are
;;; evaluated on the SAME random draws so the comparison is paired (this
;;; reduces variance compared to running two independent experiments).
;;;
;;; (define (play-once num-doors)
;;;   ... random-door ... random-door-except ... make-result ...)
;;;
;;; num-doors : the total number of doors in the game (must be >= 3)
;;;
;;; Reasoning about the switcher's outcome:
;;; - If first-choice = prize, Monty reveals any other door (a goat); the
;;;   switcher must move, and lands on a non-prize door, so loses.
;;; - If first-choice != prize, Monty must leave the prize unrevealed (he
;;;   can only reveal a goat that is not the contestant's pick).  For
;;;   3 doors, the switcher's only unrevealed non-pick door IS the prize.
;;;   For num-doors > 3 the switcher would pick uniformly among multiple
;;;   unrevealed doors -- we do not simulate that case here.
;;;
;;; We can't check exact outcomes (randomness), but we can check that the
;;; two strategies are never both winners on the same trial.
;;;
(define (play-once num-doors)
  (local [(define prize (random-door num-doors))
          (define first-choice (random-door num-doors))
          (define switch-choice
            (if (= first-choice prize)
                (random-door-except num-doors first-choice)
                prize))]
    (make-result (= first-choice prize)
                 (= switch-choice prize))))

(check-satisfied (play-once 3)
                 (lambda (r) (not (and (result-stay r)
                                       (result-switch r)))))

;;; add-result : Result Totals -> Totals
;;;
;;; Fold one trial result into the running totals.
;;;
;;; (define (add-result r t)
;;;   ... count-win ... result-stay ... totals-stay ... make-totals ...)
;;;
;;; r : the Result of one trial
;;; t : the Totals before this trial
;;;
(define (add-result r t)
  (make-totals (+ (count-win (result-stay r)) (totals-stay t))
               (+ (count-win (result-switch r)) (totals-switch t))))

(check-expect (add-result (make-result #true #false) (make-totals 10 20))
              (make-totals 11 20))
(check-expect (add-result (make-result #false #true) (make-totals 10 20))
              (make-totals 10 21))
(check-expect (add-result (make-result #true #true) (make-totals 0 0))
              (make-totals 1 1))

;;; run-trials : Natural Natural -> Totals
;;;
;;; Run num-trials Monty Hall games with num-doors doors and tally wins
;;; for each strategy.
;;;
;;; (define (run-trials num-doors num-trials)
;;;   (local [(define (loop remaining running-totals) ...)]
;;;     ... loop ... play-once ... add-result ...))
;;;
;;; num-doors  : the total number of doors per game (must be >= 3)
;;; num-trials : the number of trials to run
;;;
;;; We use an accumulator-style named loop rather than a structurally
;;; recursive count-down so that large num-trials (say, 100000+) does not
;;; blow the stack.  The accumulator is the running Totals.
;;;
(define (run-trials num-doors num-trials)
  (local [;; loop : Natural Totals -> Totals
          ;; play out remaining trials, accumulating into running-totals
          (define (loop remaining running-totals)
            (cond
              [(zero? remaining) running-totals]
              [else (loop (sub1 remaining)
                          (add-result (play-once num-doors)
                                      running-totals))]))]
    (loop num-trials (make-totals 0 0))))

;;; Structural check: 0 trials yields 0 wins.
(check-expect (run-trials 3 0) (make-totals 0 0))

;;; Structural check: any number of trials yields a Totals whose two
;;; fields sum to no more than the trial count (a trial cannot produce
;;; two wins).
(check-satisfied (run-trials 3 100)
                 (lambda (t) (<= (+ (totals-stay t) (totals-switch t)) 100)))

;;;;
;;;; Run It
;;;;

(run-trials 3 10000)

