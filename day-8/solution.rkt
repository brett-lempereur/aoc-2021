#lang racket/base

(require racket/function)
(require racket/list)
(require racket/set)
(require racket/sequence)
(require racket/stream)
(require racket/string)

;; A state, where input and output are lists of sets of active segments.
(struct state (input output) #:transparent)

;; Return a mapping of letters to their number of occurences.
(define (occurence-count s)
  (foldl
   (lambda (c acc) (hash-update acc c (curry + 1) 0))
   (make-immutable-hash)
   (string->list (string-replace s " " ""))))

;; Build a list mapping known occurence patterns to numbers.
(define (occurence-pattern s ct)
  (sort (map (lambda (c) (hash-ref ct c 0)) (string->list s)) <))

;; Create a universal translator.
(define canonical-pattern "abcefg cf acdeg acdfg bdcf abdfg abdefg acf abcdefg abcdfg")
(define canonical-count (occurence-count canonical-pattern))
(define translator
  (foldl
   (lambda (n c acc)
     (hash-set acc (occurence-pattern c canonical-count) n))
   (make-immutable-hash)
   (range 0 10)
   (string-split canonical-pattern " ")))

;; Translate the output of a state into a sequence of numbers.
(define (translate state)
  (let ([input-count (occurence-count (state-input state))])
    (map
     (lambda (output)
       (hash-ref translator (occurence-pattern output input-count)))
     (state-output state))))

;; Parse a line into a state.
(define (line->state line)
  (let ([parts (string-split line " | ")])
    (state (first parts)
           (string-split (second parts) " "))))

;; Load the input.
(define input-list
  (stream->list
   (sequence->stream
    (sequence-map
     line->state
     (in-lines (open-input-file "data/input.txt"))))))

;; Compute the solutions.
(define solution-a
  (count
   (lambda (digit)
     (or (= digit 1) (= digit 4) (= digit 7) (= digit 8)))
   (flatten (map translate input-list))))
(define solution-b
  (foldl
   +
   0
   (map
    (lambda (state)
      (foldl
       (lambda (n acc) (+ (* 10 acc) n))
       0
       (translate state)))
    input-list)))
