#lang racket/base

(require racket/list)
(require racket/set)
(require racket/stream)
(require racket/string)

;; Build a card from a sequence of numbers.
(define (list->card ns)
  (let ([rws (for/list ([i (range 0 5)])
               (list->set
                (for/list ([j (range 0 5)])
                  (list-ref ns (+ (* 5 i) j)))))]
        [cls (for/list ([i (range 0 5)])
               (list->set
                (for/list ([j (range 0 5)])
                  (list-ref ns (+ i (* 5 j))))))])
    (append rws cls)))

;; Holds if a card has a completed row.
(define (card-complete? card)
  (ormap set-empty? card))

;; Remove a number from the lines of all cards.
(define (card-mark cards n)
  (map (lambda (c) (map (lambda (l) (set-remove l n)) c)) cards))

;; Find the first card to win.
(define (first-card-to-win cards ns)
  (let ([marked (card-mark cards (car ns))])
    (if (ormap card-complete? marked)
        (list (car ns) marked)
        (first-card-to-win marked (cdr ns)))))

;; Find the last card to win.
(define (last-card-to-win remaining ns)
  (let ([marked (card-mark remaining (car ns))])
    (if (andmap card-complete? marked)
        (list (car ns) marked)
        (last-card-to-win
         (filter-not card-complete? marked)
         (cdr ns)))))

;; Compute the first result of playing the game.
(define (compute-score called-number cards)
  (let ([winner (first (filter card-complete? cards))])
    (* called-number (apply + (set->list (apply set-union winner))))))

;; Open the file and convert it to a list.
(define input-file (open-input-file "data/input-prepared.txt"))
(define input-list (stream->list (sequence->stream (in-lines input-file))))

;; Load the sequence of called numbers.
(define called-numbers
  (map string->number (string-split (string-trim (first input-list)) ",")))

;; Load the sequence of cards.
(define cards
  (map
   list->card
   (map
    (lambda (l) (map string->number (string-split l " ")))
    (filter non-empty-string? (cdr input-list)))))

;; Compute the results.
(define first-solution (first-card-to-win cards called-numbers))
(define second-solution (last-card-to-win cards called-numbers))

;; Compute the first result.
(printf "The first card to win has score: ~a\n" (apply compute-score first-solution))
(printf "The last card to win has score: ~a\n" (apply compute-score second-solution))
