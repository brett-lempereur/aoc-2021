#lang racket/base

(require racket/function)
(require racket/hash)
(require racket/list)
(require racket/sequence)

(require memo)

;; Regular expression patterns for polymer chains and insertions.
(define polymer-re #px"^([A-Z]+)$")
(define insertion-re #px"^([A-Z][A-Z]) -> ([A-Z])$")

;; Parse a polymer chain from an input file.
(define (file->polymer filename)
  (let ([lines (sequence->list (in-lines (open-input-file filename)))])
    (string->list
     (first
      (map
       second
       (filter list? (map (curry regexp-match polymer-re) lines)))))))

;; Parse a list of insertions from an input file.
(define (file->insertions filename)
  (let ([lines (sequence->list (in-lines (open-input-file filename)))])
    (foldl
     (lambda (i acc)
       (hash-set acc (string->list (second i)) (string-ref (third i) 0)))
     (hash)
     (filter list? (map (curry regexp-match insertion-re) lines)))))

;; Recursively expand a pair of elements and return a map of elements to
;; their frequency in the expanded chain.
;;
;; The second element will intentionally be undercounted by one for the
;; top-level call to this function.
(define/memoize (expand x y im n)
  (if
   (<= n 0)
   (hash x 1)
   (let ([i (hash-ref im (list x y))])
     (hash-union
      (expand x i im (sub1 n))
      (expand i y im (sub1 n))
      #:combine +))))

;; Recursively expand a polymer chain and return a map of elements to
;; their frequency in the expanded chain.
(define (expand-chain p im n)
  (foldl
   (lambda (f acc)
     (hash-union acc f #:combine +))
   (hash (last p) 1)
   (for/list ([i (range 0 (length p))] [j (range 1 (length p))])
     (let ([x (list-ref p i)] [y (list-ref p j)])
       (expand x y im n)))))

;; Load the inputs.
(define polymer (file->polymer "data/input.txt"))
(define insertions (file->insertions "data/input.txt"))

;; Compute the difference between the highest and lowest frequency
;; element after polymer chain expansion.
(define (frequency-difference p im n)
  (let* ([output (expand-chain p im n)]
         [mf (apply max (hash-values output))]
         [lf (apply min (hash-values output))])
    (- mf lf)))

;; Compute and display both solutions.
(printf "Frequency difference after 10 steps: ~a\n"
        (frequency-difference polymer insertions 10))
(printf "Frequency difference after 40 steps: ~a\n"
        (frequency-difference polymer insertions 40))
