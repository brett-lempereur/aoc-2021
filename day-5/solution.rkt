#lang racket/base

(require racket/match)
(require racket/list)
(require racket/sequence)
(require racket/stream)

;; Data structures.
(struct line (a b) #:transparent)
(struct vec (x y) #:transparent)

;; Expand a line into a list of its points.
(define (line->points line)
  (match-let ([(vec xa ya) (line-a line)]
              [(vec xb yb) (line-b line)])
    (let* ([md (max (abs (- xb xa)) (abs (- yb ya)))]
           [dx (/ (- xb xa) md)]
           [dy (/ (- yb ya) md)])
      (for/list ([s (range 0 (+ 1 md))])
        (vec (+ xa (* s dx)) (+ ya (* s dy)))))))

;; Holds if the given line is horizontal or vertical.
(define (horizontal-or-vertical? line)
  (match-let ([(vec a b) (line-a line)]
              [(vec c d) (line-b line)])
    (or (= a c) (= b d))))

;; Return a hash-map that maps grid coordinates to the number of lines
;; that overlap them.
(define (lines->grid lines)
  (foldl
   (lambda (p acc)
     (hash-update acc p (lambda (c) (+ 1 c)) 0))
   (make-immutable-hash)
   (flatten (map line->points lines))))

;; Count the number of cells that have more than one intersection.
(define (grid-danger grid)
  (count (lambda (p) (> p 1)) (hash-values grid)))

;; Parse a text line into a line.
(define (line->line s)
  (let* ([match (regexp-match #px"(\\d+),(\\d+) -> (\\d+),(\\d+)" s)]
         [match-numbers (map string->number (drop match 1))])
    (line (apply vec (take match-numbers 2))
          (apply vec (drop match-numbers 2)))))

;; Load the input file.
(define input-lines
  (stream->list
   (sequence->stream
    (sequence-map line->line (in-lines (open-input-file "data/input.txt"))))))

;; Compute and display the outputs for both solutions.
(define solution-a
  (grid-danger (lines->grid (filter horizontal-or-vertical? input-lines))))
(define solution-b
  (grid-danger (lines->grid input-lines)))
(printf "Danger score for horizontal or vertical only: ~a\n" solution-a)
(printf "Danger score for all lines: ~a\n" solution-b)
