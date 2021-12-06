#lang racket/base

(require racket/list)
(require racket/stream)
(require racket/string)

(define lifespan-normal 6)
(define lifespan-birth 8)

;; Process a tick.
(define (tick census)
  (list-update
   (append (drop census 1) (take census 1))
   lifespan-normal
   (lambda (v) (+ v (first census)))))

;; Simulate the lifecycle for a given number of days.
(define (simulate census days)
  (foldl (lambda (_ acc) (tick acc)) census (range 0 days)))

;; Calculate the number of fish alive after a given number of
;; days.
(define (result census days)
  (foldl + 0 (simulate census days)))

;; Build a census mapping lifespans to the number of fish with
;; that lifespan.
(define (numbers->census ns)
  (let ([census (make-vector (+ lifespan-birth 1) 0)])
    (for ([n ns])
      (vector-set! census n (+ (vector-ref census n) 1)))
    (vector->list census)))

;; Load the input.
(define input-filename "data/input.txt")
(define input-list
  (flatten
   (map
    (lambda (l) (map string->number (string-split l ",")))
    (stream->list
     (sequence->stream
      (in-lines (open-input-file input-filename)))))))
(define input-census (numbers->census input-list))

;; Compute and display both solutions.
(define solution-a (result input-census 80))
(define solution-b (result input-census 256))
(printf "Lanternfish alive after 80 days: ~a\n" solution-a)
(printf "Lanternfish alive after 256 days: ~a\n" solution-b)
