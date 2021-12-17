#lang racket/base

(require racket/generator)
(require racket/list)
(require racket/match)
(require racket/math)
(require racket/sequence)
(require racket/stream)

;; A point in two-dimensional space.
(struct point (x y) #:transparent)

;; An area in two-dimensional space.
(struct area (a b) #:transparent)

;; A probe with a position and velocity in two-dimensional space.
(struct probe (x y u v) #:transparent)

;; Holds if the probe is within a target area.
(define (in-area? target prb)
  (match-let ([(area (point txa tya) (point txb tyb)) target]
              [(probe x y _ _) prb])
    (and (>= x txa) (<= x txb) (<= y tya) (>= y tyb))))

;; Holds if it is possible for the probe to reach a target area.
(define (can-reach? target prb)
  (match-let ([(area (point txa tya) (point txb tyb)) target]
              [(probe x y u v) prb])
    (not (or (and (<= u 0) (< x txa))
             (and (<= v 0) (< y tyb))
             (> x txb)))))

;; Update the position and velocity of a probe.
(define (step prb)
  (match-let ([(probe x y u v) prb])
    (probe (+ x u)
           (+ y v)
           (cond [(> u 0) (sub1 u)] [(< u 0) (add1 u)] [(= u 0) u])
           (sub1 v))))

;; Simulate a probe, returning a list of all points visited for initial
;; trajectories that hit the target, an empty list otherwise.
(define (simulate target prb)
  (let loop ([p prb] [path null])
    (let ([pos (probe->position p)])
      (cond
        [(in-area? target p) (cons pos path)]
        [(can-reach? target p) (loop (step p) (cons pos path))]
        [else null]))))

;; Create a point from the current position of a probe.
(define (probe->position prb)
  (point (probe-x prb) (probe-y prb)))

;; Return a sequence of initial velocities that eventually hit the target.
(define (simulation-valid-inputs target us vs)
  (sequence->stream
   (in-generator
    (for* ([u us] [v vs])
      (when (not (null? (simulate target (probe 0 0 u v))))
        (yield (list u v)))))))

;; Simulate a set of valid trajectories and find the highest point that
;; hits the target.
(define (highest-point target uvs)
  (exact-floor
   (sequence-fold
    (lambda (highest path)
      (max highest (foldl max -inf.0 (map point-y path))))
    -inf.0
    (in-generator
     (for ([uv uvs])
       (yield (simulate target (probe 0 0 (first uv) (second uv)))))))))

;; The target area given as an input.
(define input-target (area (point 34 -186) (point 67 -215)))

;; Compute the initial velocities that can hit the target and the
;; highest point on any of them.
(define trajectories-that-hit-target
  (simulation-valid-inputs input-target (range 0 68) (range -250 250)))
(define highest-point-that-hit-target
  (highest-point input-target trajectories-that-hit-target))

;; Display the results.
(printf "Highest point on a trajectory that hits the target: ~a\n"
        highest-point-that-hit-target)
(printf "Number of trajectories that can hit the target: ~a\n"
        (stream-length trajectories-that-hit-target))
