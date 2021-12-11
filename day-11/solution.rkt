#lang racket/base

(require racket/function)
(require racket/list)
(require racket/match)
(require racket/set)
(require racket/stream)

;; A cell.
(struct cell (x y) #:transparent)

;; Load the input file.
(define input-lines
  (stream->list
   (sequence->stream
    (in-lines (open-input-file "data/input.txt")))))

;; Build a row-major array from the input and calculate its width
;; and height.
(define input-matrix
  (flatten
   (map
    (lambda (l) (map (compose string->number string) l))
    (map string->list input-lines))))
(define input-width (string-length (first input-lines)))
(define input-height (length input-lines))

;; The maximum energy an octopus has before it flashes.
(define max-energy 9)

;; Returns the matrix index of a cell.
(define (cell->index c)
  (+ (* input-height (cell-y c)) (cell-x c)))

;; Get the value of a cell in a matrix.
(define (get-cell m c)
  (list-ref m (cell->index c)))

;; Set the value of a cell in the matrix.
(define (set-cell m c v)
  (list-set m (cell->index c) v))

;; Update the value of a cell in a matrix.
(define (update-cell m c u)
  (set-cell m c (u (get-cell m c))))

;; Holds if the given cell is within bounds.
(define (in-bounds? c)
  (match-let ([(cell x y) c])
    (and (>= x 0) (< x input-width) (>= y 0) (< y input-height))))

;; Get the list of within-bounds cells adjacent to a given cell.
(define (adjacent c)
  (match-let ([(cell x y) c])
    (filter
     in-bounds?
     (map
      (lambda (d) (cell (+ x (car d)) (+ y (cdr d))))
      (list '(-1 . -1) '(0 . -1) '(1 . -1) '(1 . 0) '(1 . 1) '(0 . 1)
            '(-1 . 1) '(-1 . 0))))))

;; Holds if the given cell has exceeded the maximum energy.
(define (primed? m c)
  (> (get-cell m c) max-energy))

;;;
;;; Phases
;;;

;; Increment the energy level of each octopus by one.
(define (increment-all m)
  (map add1 m))

;; Return a list of the position of all octopuses that are ready to
;; flash.
(define (matrix->flashing m)
  (filter-not
   null?
   (for*/list ([x (range 0 input-width)] [y (range 0 input-height)])
     (if (primed? m (cell x y)) (cell x y) null))))

;; Reset all flashing octopuses to the lowest energy level.
(define (reset-flashing m f)
  (foldl (lambda (t macc) (set-cell macc t 0)) m f))

;; Increment all cells adjacent to a flashing octopus.
(define (increment-adjacent-to-flashing m f)
  (let ([as (flatten (map adjacent f))])
    (foldl (lambda (t macc) (update-cell macc t add1)) m as)))

;; Given a matrix keep flashing until there are no further octopuses to
;; flash.
(define (flash-until-done m [v (set)])
  (let* ([f (matrix->flashing m)]
         [t (set->list (set-subtract (list->set f) v))])
    (if
     (empty? t)
     (values m v)
     (flash-until-done
      (increment-adjacent-to-flashing m t)
      (set-union v (list->set t))))))

;; Step the simulation.
(define (step m)
  (let-values ([(m v) (flash-until-done (increment-all m))])
    (values (reset-flashing m (set->list v)) v)))

;;;
;;; Execution
;;;

;; Count how many octopuses flash over a given number of steps.
(define (count-flashes m n [a 0])
  (if
   (<= n 0)
   a
   (let-values ([(m v) (step m)])
     (count-flashes m (sub1 n) (+ a (set-count v))))))

;; Count the number of steps until all octopuses flash.
(define (steps-until-synchronise m [n 1])
  (let-values ([(m v) (step m)])
    (if
     (= (set-count v) (length m))
     n
     (steps-until-synchronise m (add1 n)))))

;;;
;;; Solutions
;;;

(printf "Number of flashes after 100 steps: ~a\n"
        (count-flashes input-matrix 100))
(printf "First step where all flashes synchronise: ~a\n"
        (steps-until-synchronise input-matrix))
