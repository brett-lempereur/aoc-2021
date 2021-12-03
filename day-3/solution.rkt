#lang racket

;; Get the list of numbers, also count the number of bits (assuming
;; each line encodes the same number of bits).
(define input-lines
  (sequence->list (in-lines (open-input-file "data/input.txt"))))
(define input-numbers
  (map (lambda (s) (string->number s 2)) input-lines))
(define input-bits
  (string-length (first input-lines)))

;; Determines either the most-common or least-common bit at an index
;; depending on a comparator.
(define (common-bit ns i c tie)
  (let* ([one-count (count (lambda (n) (bitwise-bit-set? n i)) ns)]
         [zero-count (- (length ns) one-count)])
    (cond
      [(c zero-count one-count) 0]
      [(c one-count zero-count) 1]
      [else tie])))

;; Determines the most-common-bit.
(define (most-common-bit ns i tie)
  (common-bit ns i > tie))

;; Determines the least-common bit.
(define (least-common-bit ns i tie)
  (common-bit ns i < tie))

;; Compute a rate.
(define (compute-rate ns c tie)
  (foldr
   (lambda (i acc) (bitwise-ior (arithmetic-shift acc 1) (c ns i tie)))
   0
   (range 0 input-bits)))

;; Compute a system rating.
(define (compute-rating ns c tie)
  (car
   (foldr
    (lambda (i rem)
      (if
       (= (length rem) 1)
       rem
       (let ([target (c rem i tie)])
         (filter
          (lambda (n) (= (bitwise-bit-field n i (+ i 1)) target))
          rem))))
    ns
    (range 0 input-bits))))

;; Compute the gamma and epsilon rate.
(define gamma-rate (compute-rate input-numbers most-common-bit 0))
(define epsilon-rate (compute-rate input-numbers least-common-bit 0))

;; Compute the system ratings.
(define oxygen-generator-rating (compute-rating input-numbers most-common-bit 1))
(define co2-scrubber-rating (compute-rating input-numbers least-common-bit 0))

;; Show the gamma and epsilon rate.
(fprintf
 (current-output-port)
 "Gamma rate=~a, Epsilon rate=~a: ~a\n"
 gamma-rate
 epsilon-rate
 (* gamma-rate epsilon-rate))

;; Show the oxygen generator and CO2 scrubber rating.
(fprintf
 (current-output-port)
 "Oxygen Generator rating=~a, CO2 Scrubber rating=~a: ~a\n"
 oxygen-generator-rating
 co2-scrubber-rating
 (* oxygen-generator-rating co2-scrubber-rating))
