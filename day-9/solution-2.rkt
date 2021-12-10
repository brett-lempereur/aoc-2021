#lang racket/base

;;; This is an iterative approach that for each row:
;;;
;;; 1. Colours each contiguous region bounded by nines with a unique
;;; integer (each cell visited once).
;;; 2. For each coloured cell (a) on the row, if the colour of the cell
;;; (b) above it is different, update the colour of all (a) to (b).
;;;
;;; Each cell is then assigned to its basin.

(require racket/list)
(require racket/match)
(require racket/set)
(require racket/stream)
(require racket/string)

;; A position.
(struct pos (x y) #:transparent)

;; A colouring.
(struct colouring (n p->i i->p) #:transparent)

;; Load the input file.
(define input-lines
  (sequence->stream (in-lines (open-input-file "data/input.txt"))))

;; Create an input matrix which is a row-major vector of cells.
(define input-size
  (string-length (string-trim (stream-first input-lines))))
(define input-matrix
  (list->vector
   (map
    (compose string->number string)
    (flatten
     (stream->list
      (stream-map (compose string->list string-trim) input-lines))))))

;; Create an empty colouring.
(define (make-colouring)
  (colouring 0 (make-immutable-hash) (make-immutable-hash)))

;; Return the value of the cell at coordinates `x` and `y`.
(define (cell m p)
  (vector-ref m (+ (* input-size (pos-y p)) (pos-x p))))

;; The value of a basin edge.
(define (edge? m p) (= (cell m p) 9))

;; Holds if the given coordinates are within bounds.
(define (in-bounds? p)
  (and (>= (pos-x p) 0) (< (pos-x p) input-size)
       (>= (pos-y p) 0) (< (pos-y p) input-size)))

;; Functions to compute adjacent cells.
(define (pos-left p) (pos (- (pos-x p) 1) (pos-y p)))
(define (pos-right p) (pos (+ (pos-x p) 1) (pos-y p)))
(define (pos-up p) (pos (pos-x p) (- (pos-y p) 1)))

;; Add a colour for a cell.
(define (set-colour c p)
  (match-let ([(colouring n p->i i->p) c])
    (colouring
     n
     (hash-set p->i p n)
     (hash-update i->p n (lambda (s) (set-add s p)) (set)))))

;; Increment the colour.
(define (next-colour c)
  (colouring (+ 1 (colouring-n c)) (colouring-p->i c) (colouring-i->p c)))

;; Combine two colours.
(define (combine-colour c a b)
  (match-let* ([(colouring n p->i i->p) c]
               [ia (hash-ref p->i a)]
               [ib (hash-ref p->i b)]
               [sb (hash-ref i->p ib)])
    (colouring
     n
     (foldl (lambda (p acc) (hash-set acc p ia)) p->i (set->list sb))
     (hash-update (hash-remove i->p ib) ia (lambda (s) (set-union s sb))))))

;; Colour all contiguous cells in a row that are part of the same basin.
(define (colour-row m c p)
  (if
   (in-bounds? p)
   (if
    (edge? m p)
    (colour-row m (next-colour c) (pos-right p))
    (colour-row m (set-colour c p) (pos-right p)))
   c))

;; Combine all contiguous regions on a row with the row above.
(define (combine-row m c p)
  (match-let ([(colouring n p->i i->p) c]
              [pu (pos-up p)])
    (if
     (and (in-bounds? p) (in-bounds? pu))
     (if
      (or (edge? m p) (edge? m pu))
      (combine-row m c (pos-right p))
      (if (not (= (hash-ref p->i p) (hash-ref p->i pu)))
          (combine-row m (combine-colour c pu p) (pos-right p))
          (combine-row m c (pos-right p))))
     c)))

;; Compute the final colouring of the matrix.
(define matrix-colouring
  (foldl
   (lambda (y c)
     (let ([p (pos 0 y)])
       (combine-row input-matrix (colour-row input-matrix c p) p)))
   (make-colouring)
   (range 0 100)))

;; Find the size of the three largest basins.
(define solution-b
  (foldl
   *
   1
   (take
    (sort (map set-count (hash-values (colouring-i->p matrix-colouring))) >)
    3)))

;; Display the solution.
(printf "The product of the three largest basin size: ~a\n" solution-b)
