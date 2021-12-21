#lang racket/base

(require racket/generator)
(require racket/hash)
(require racket/list)
(require racket/match)
(require racket/math)

(require memo)

;;;
;;; Data model
;;;

;; A player.
(struct player (position score) #:transparent)

;;;
;;; Deterministic Game
;;;

;; Return a generator that yields deterministic dice rolls.
(define (deterministic-die)
  (sequence->generator (in-cycle (in-inclusive-range 1 100))))

;; Holds if a player has won the deterministic variant of the game.
(define (player-won-deterministic? p)
  (>= (player-score p) 1000))

;; Move a player at some position and add to their score.
(define (move p n)
  (match-let ([(player position score) p])
    (let* ([sum (modulo (+ position n) 10)]
           [np (if (= 0 sum) 10 sum)])
      (player np (+ score np)))))

;; Play a game until a player loses, returning the player structures and
;; the number of times the dice was rolled.
(define (play players die [rolls 0])
  (if
   (ormap player-won-deterministic? players)
   (values players rolls)
   (let ([current (first players)]
         [pending (rest players)])
     (play
      (append pending (list (move current (+ (die) (die) (die)))))
      die
      (+ rolls 3)))))

;; Return the score of the losing player.
(define (losing-score players)
  (exact-floor (foldl min +inf.0 (map player-score players))))

;;;
;;; Quantum game
;;;

;; Frequences of dice rolls for the quantum game variant.
(define roll-frequencies (hash 3 1 4 3 5 6 6 7 7 6 8 3 9 1))

;; Play the quantum variant of the game.
(define (play-quantum ap bp as bs)
  (cond
    [(>= as 21) (values 1 0)]
    [(>= bs 21) (values 0 1)]
    [else
     (for/fold ([aw 0] [bw 0])
               ([roll (hash-keys roll-frequencies)])
       (let* ([f (hash-ref roll-frequencies roll)]
              [n (modulo (+ ap roll) 10)]
              [nap (if (= n 0) 10 n)]
              [nas (+ as nap)])
         (let-values ([(lbw law) (play-quantum bp nap bs nas)])
           (values (+ aw (* law f)) (+ bw (* lbw f))))))]))

;;;
;;; Part one
;;;

;; Compute the results for part one.
(define part-one-players
  (list (player 9 0) (player 10 0)))
(define-values (part-one-outcomes part-one-rolls)
  (play part-one-players (deterministic-die)))
(define part-one-losing-score
  (losing-score part-one-outcomes))
(define part-one-result
  (* part-one-losing-score part-one-rolls))

;; Display the result for part one.
(printf "Losing score multiplied by die rolls: ~a\n" part-one-result)

;;;
;;; Part two
;;;

;; Compute the result for part two.
(define part-two-result
  (call-with-values (lambda () (play-quantum 9 10 0 0)) max))

;; Display the result for part two.
(printf "Universes in which most frequent winner wins: ~a\n" part-two-result)
