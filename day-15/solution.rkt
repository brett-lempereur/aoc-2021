#lang racket/base

(require racket/list)
(require racket/match)
(require racket/sequence)

(require graph)

;; A grid cell.
(struct cell (x y v) #:transparent)

;; A repeat of the grid.
(struct repeat (x y n) #:transparent)

;; Load a file as a list of lists of risk scores.
(define (file->rows filename)
  (let ([lines (sequence->list (in-lines (open-input-file filename)))])
    (map
     (lambda (l) (map (compose string->number string) l))
     (map string->list lines))))

;; Load a file as a list of cells.
(define (file->cells filename)
  (let* ([rows (file->rows filename)])
    (flatten
     (map
      (lambda (y r) (map (lambda (x v) (cell x y v)) (range (length r)) r))
      (range 0 (length rows))
      rows))))

;; Convert a list of cells to a list of edges.
(define (cells->edges cells)
  (if
   (empty? cells)
   (list)
   (match-let ([(cell x y v) (first cells)] [tail (rest cells)])
     (append
      (map (lambda (s t) (list v (list (+ x s) (+ y t)) (list x y)))
           (list 1 -1 0 0) (list 0 0 1 -1))
      (cells->edges tail)))))

;; Offset a list of cells and increment their values.
(define (offset-cells cells x y n)
  (map
   (lambda (c)
     (match-let* ([(cell p q v) c] [nv (+ v n)])
       (cell
        (+ x p)
        (+ y q)
        (modulo (if (> nv 9) (add1 nv) nv) 10))))
   cells))

;; Expand a map by repeating a list of cells.
(define (expand-map cells repeats)
  (flatten
   (map
    (lambda (r)
      (offset-cells cells (repeat-x r) (repeat-y r) (repeat-n r)))
    repeats)))

;; Find the risk score of the shortest path from (0, 0) to target.
(define (shortest-path-risk cells target)
  (let ([g (weighted-graph/directed (cells->edges cells))])
    (let-values ([(d p) (dijkstra g (list 0 0))])
      (hash-ref d target))))

;; The input cells.
(define input-cells (file->cells "data/input.txt"))

;; The expanded map for part two.
(define expanded-cells
  (expand-map
   input-cells
   (for*/list ([x (range 0 5)] [y (range 0 5)])
     (repeat (* 100 x) (* 100 y) (+ x y)))))

(shortest-path-risk input-cells (list 99 99))
(shortest-path-risk expanded-cells (list 499 499))
