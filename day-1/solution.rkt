#lang racket

(require racket/generator)

(define input-filename "data/readings.txt")
(define input-lines (in-lines (open-input-file input-filename)))
(define input (sequence->stream (sequence-map string->number (sequence->stream input-lines))))

;; Returns a lazy sequence of `size`-valued windows over an `input` sequence.
(define (windows input size)
  (apply
   in-parallel
   (map
    (lambda (i) (sequence-tail input i))
    (range 0 size))))

;; Returns the number of rising edges of the sum of `size`-valued windows over
;; an `input` sequence.
(define (windowed-rising-edge-count input size)
  (sequence-count
   (lambda (left right) (< left right))
   (windows (sequence-map + (windows input size)) 2)))

;; Prettily formats output.
(define (print-result input size)
  (fprintf (current-output-port)
           "Rising edge count (window-size=~a): ~a.\n"
           size
           (windowed-rising-edge-count input size)))

;; Compute and display the output for the AoC cases.
(print-result input 1)
(print-result input 3)
