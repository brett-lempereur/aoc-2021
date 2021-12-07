#lang racket/base

(require racket/list)
(require racket/stream)
(require racket/string)

;; Computes the linear fuel cost for a proposed position.
(define (linear-score n ns)
  (foldl + 0 (map (lambda (h) (abs (- h n))) ns)))

;; Computes the divergent (1+2+3+...) score for a proposed position.
(define (divergent-score n ns)
  (foldl + 0 (map
              (lambda (h)
                (let ([d (abs (- h n))])
                  (/ (* d (+ d 1)) 2)))
              ns)))

;; Brute-force search for an optimal position under some given scoring
;; function.
(define (search ns score)
  (let ([min-n (foldl min +inf.0 ns)]
        [max-n (foldl max -inf.0 ns)])
    (foldl
     min
     +inf.0
     (map
      (lambda (n) (score n ns))
      (range min-n (+ 1 max-n))))))

;; Load the input.
(define input-lines
  (sequence->stream (in-lines (open-input-file "data/input.txt"))))
(define input-positions
  (map string->number (string-split (stream-first input-lines) ",")))

;; Calculate the first solution.
(printf "Minimum linear movement fuel cost: ~a\n"
        (search input-positions linear-score))
(printf "Minimum divergent movement fuel cost: ~a\n"
        (search input-positions divergent-score))
