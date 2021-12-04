#lang racket/base

(require racket/list)
(require racket/set)
(require racket/stream)
(require racket/string)

;; The column and row size of a card.
(define card-size 5)

;; Build a card from a list of numbers.
;;
;; We treat the input as a row-major array containing a `card-size` grid
;; of numbers, and represent the card as a list of sets of all row and
;; column lines.
(define (list->card ns)
  (append
   (for/list ([i (range 0 card-size)])
     (list->set
      (for/list ([j (range 0 card-size)])
        (list-ref ns (+ (* card-size i) j)))))
   (for/list ([i (range 0 card-size)])
     (list->set
      (for/list ([j (range 0 card-size)])
        (list-ref ns (+ i (* card-size j))))))))

;; Holds if a card has at least one completed line.
(define (card-complete? card)
  (ormap set-empty? card))

;; Remove a number from the lines of all cards.
(define (card-mark cards n)
  (map (lambda (c) (map (lambda (l) (set-remove l n)) c)) cards))

;; Return the single winning card from a list of marked cards.
(define (winning-card cards)
  (let ([winners (filter card-complete? cards)])
    (if (= 1 (length winners))
        (car winners)
        (error "Multiple completed cards in input"))))

;; Find the first card to win.
(define (first-card-to-win cards ns)
  (let ([marked (card-mark cards (car ns))])
    (if (ormap card-complete? marked)
        (list (car ns) (winning-card marked))
        (first-card-to-win marked (cdr ns)))))

;; Find the last card to win.
(define (last-card-to-win remaining ns)
  (let* ([marked (card-mark remaining (car ns))]
         [incomplete (filter-not card-complete? marked)])
    (if (empty? incomplete)
        (list (car ns) (winning-card marked))
        (last-card-to-win incomplete (cdr ns)))))

;; Compute the first result of playing the game.
(define (compute-score called-number winner)
  (* called-number
     (apply + (set->list (apply set-union winner)))))

;; Open the file and convert it to a list.
(define input-file (open-input-file "data/input-prepared.txt"))
(define input-list (stream->list (sequence->stream (in-lines input-file))))

;; Load the called numbers and cards.
(define called-numbers
  (map string->number (string-split (string-trim (first input-list)) ",")))
(define cards
  (map
   list->card
   (map
    (lambda (l) (map string->number (string-split l " ")))
    (filter non-empty-string? (cdr input-list)))))

;; Compute and display the results.
(define first-solution (first-card-to-win cards called-numbers))
(define second-solution (last-card-to-win cards called-numbers))
(printf "The first card to win has score: ~a\n" (apply compute-score first-solution))
(printf "The last card to win has score: ~a\n" (apply compute-score second-solution))
