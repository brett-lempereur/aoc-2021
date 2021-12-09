#lang racket/base

(require racket/function)
(require racket/list)
(require racket/set)
(require racket/stream)
(require racket/string)

;; A position.
(struct pos (x y) #:transparent)

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

;; Precompute a list of all valid positions in the matrix.  This
;; solution will be extremely brute force.
(define input-positions
  (flatten
   (for/list ([x (range 0 input-size)])
     (for/list ([y (range 0 input-size)])
       (pos x y)))))

;; Return the value of the cell at coordinates `x` and `y`.
(define (cell m p)
  (vector-ref m (+ (* input-size (pos-y p)) (pos-x p))))

;; Holds if the given coordinates are within bounds.
(define (in-bounds? p)
  (and (>= (pos-x p) 0) (< (pos-x p) input-size)
       (>= (pos-y p) 0) (< (pos-y p) input-size)))

;; Return a list of the within bound adjacent cells of a given cell.
(define (adjacent-cells p c)
  (filter-not
   null?
   (map
    (lambda (d)
      (let ([np (pos (+ (pos-x p) (pos-x d)) (+ (pos-y p) (pos-y d)))])
        (if (c np) np null)))
    (list (pos -1 0) (pos 1 0) (pos 0 -1) (pos 0 1)))))

;; Holds if the given cell is a local minimum.
(define (local-minimum? m p)
  (let ([v (cell m p)])
    (andmap
     (lambda (a) (< v (cell m a)))
     (adjacent-cells p in-bounds?))))

;; The list of all local minimums.
(define local-minimums
  (filter
   (lambda (p) (local-minimum? input-matrix p))
   input-positions))

;; Compute the risk level of a given point.
(define (risk-level m p)
  (if (local-minimum? m p)
      (+ 1 (cell m p))
      0))

;; Find the basin that contains a starting point.
(define (find-basin m p v)
  (let* ([adjacent (adjacent-cells p in-bounds?)]
         [not-edge (filter (lambda (c) (not (= 9 (cell m c)))) adjacent)]
         [not-visited (filter (lambda (c) (not (set-member? v c))) not-edge)])
    (let ([uv (set-union v (list->set not-visited))])
      (if
       (empty? not-visited)
       v
       (foldl
        (lambda (np acc)
          (if (and (set-member? acc np) (not (set-member? not-visited np)))
              acc
              (find-basin m np acc)))
        uv
        not-visited)))))

;; Find all basins that contain a set of candidate points.
(define (find-basins m ps)
  (sort
   (set->list
    (foldl
     (lambda (p acc)
       (if
        (ormap (lambda (bps) (set-member? bps p)) (set->list acc))
        acc
        (set-add acc (find-basin m p (set)))))
     (set)
     ps))
   >
   #:key
   (lambda (s) (set-count s))))

;; Compute the sorted list of basins.
(define sorted-basins (find-basins input-matrix local-minimums))

;; Compute the solutions.
(define solution-a
  (foldl
   +
   0
   (map (lambda (p) (risk-level input-matrix p)) local-minimums)))
(define solution-b
  (apply * (map set-count (take sorted-basins 3))))

;; Show the solutions.
(printf "Risk score: ~a\n" solution-a)
(printf "Multiplied size of three largest basins: ~a\n" solution-b)
