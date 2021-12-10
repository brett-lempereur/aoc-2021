#lang racket/base

(require racket/function)
(require racket/list)
(require racket/sequence)
(require racket/stream)
(require racket/string)

;; Load the input file.
(define input-lines
  (sequence->stream
   (sequence-map
    string->list
    (sequence-filter
     non-empty-string?
     (in-lines (open-input-file "data/input.txt"))))))

;; Symbol lookup tables.
(define opening->closing
  (hash #\< #\> #\( #\) #\[ #\] #\{ #\}))
(define closing->opening
  (hash #\> #\< #\) #\( #\] #\[ #\} #\{))

;; A mapping of closing symbols to their error scores.
(define error-scores
  (hash #\) 3 #\] 57 #\} 1197 #\> 25137))

;; A mapping of closing symbols to their completion scores.
(define completion-scores
  (hash #\) 1 #\] 2 #\} 3 #\> 4))

;; Holds if a value is an incomplete stack.
(define (incomplete-stack? s)
  (and (cons? s) (not (empty? s))))

;; Holds if a value is an error.
(define error? char?)

;; Scan a line, returning an empty stack if a line is balanced, a
;; partial stack if it is unbalanced, or an error if it is
;; corrupted.
(define (scan line [stack null])
  (if
   (empty? line)
   stack
   (let ([head (first line)] [tail (rest line)])
     (if
      (hash-has-key? closing->opening head)
      (if (and (not (empty? stack))
               (eq? (hash-ref closing->opening head) (car stack)))
          (scan tail (cdr stack))
          head)
      (scan tail (cons head stack))))))

;; Return the string that would complete a partial input.
(define (complete stack)
  (map (curry hash-ref opening->closing) stack))

;; Return a stream of error scores of the input.
(define (input->error-scores input)
  (stream-map
   (curry hash-ref error-scores)
   (stream-filter error? (stream-map scan input))))

;; Return a stream of completion scores of the input.
(define (input->completion-scores input)
  (stream-map
   (curry
    foldl
    (lambda (c acc) (+ (hash-ref completion-scores c) (* 5 acc))) 0)
   (stream-map
    complete
    (stream-filter incomplete-stack? (stream-map scan input)))))

;; Compute the error score of an input.
(define (error-score input)
  (stream-fold + 0 (input->error-scores input)))

;; Compute the completion score of the input.
(define (completion-score input)
  (let* ([s (sort (stream->list (input->completion-scores input)) <)]
         [sl (length s)]
         [mid ((if (even? sl) ceiling floor) (/ sl 2))])
    (list-ref s mid)))

;; Compute and display both solutions.
(define solution-a (error-score input-lines))
(define solution-b (completion-score input-lines))
(printf "The error score is: ~a\n" solution-a)
(printf "The completion score is: ~a\n" solution-b)
