#lang racket/base

(require racket/draw)
(require racket/function)
(require racket/list)
(require racket/match)
(require racket/class)
(require racket/sequence)

;;;
;;; Data structures
;;;

;; A point.
(struct point (x y) #:transparent)

;; A fold.
(struct fold (axis n) #:transparent)

;;;
;;; Parsing
;;;

;; Regular expression patterns for points and folds.
(define point-re #px"^(\\d+),(\\d+)$")
(define fold-re #px"^fold along ([xy])=(\\d+)$")

;; Parse a list of points from an input file.
(define (file->points filename)
  (let ([lines (sequence->list (in-lines (open-input-file filename)))])
    (map
     (lambda (m) (point (string->number (second m)) (string->number (third m))))
     (filter list? (map (curry regexp-match point-re) lines)))))

;; Parse a list of folds from an input file.
(define (file->folds filename)
  (let ([lines (sequence->list (in-lines (open-input-file filename)))])
    (map
     (lambda (m) (fold (string->symbol (second m)) (string->number (third m))))
     (filter list? (map (curry regexp-match fold-re) lines)))))

;;;
;;; Folding
;;;

;; Mirror a point vertically about a line upwards.
(define (mirror-vertical n p)
  (match-let ([(point x y) p])
    (if (<= y n) p (point x (- n (- y n))))))

;; Mirror a point horizontally about a line rightwards.
(define (mirror-horizontal n p)
  (match-let ([(point x y) p])
    (if (<= x n) p (point (- n (- x n)) y))))

;; Find the visible points by removing duplicates.
(define (filter-visible p)
  (remove-duplicates p))

;; Apply a fold to a list of points, returning only those points that
;; are visible after the fold.
(define (fold-points p f)
  (filter-visible
   (case (fold-axis f)
     [(x) (map (curry mirror-horizontal (fold-n f)) p)]
     [(y) (map (curry mirror-vertical (fold-n f)) p)]
     [else (error 'fold-points "unexpected fold direction ~a" (fold-axis f))])))

;; Apply a list of folds to a list of points, returning only those
;; points that are visible after all folds.
(define (apply-folds p f)
  (foldl (lambda (u acc) (fold-points acc u)) p f))

;;;
;;; Drawing
;;;

(define (draw-points filename p)
  (let* ([target (make-bitmap 40 6)]
         [dc (new bitmap-dc% [bitmap target])])
    (for-each (lambda (u) (send dc draw-point (point-x u) (point-y u)))
              p)
    (send target save-file filename 'png)))

;;;
;;; Solutions
;;;

;; The input filename.
(define input-filename "data/input.txt")

;; Load the list of points and folds.
(define points (file->points input-filename))
(define folds (file->folds input-filename))

;; Apply the first fold and display the number of visible points.
(define after-first-fold (fold-points points (first folds)))
(printf "Number of points visible after first fold: ~a\n"
        (length after-first-fold))

;; Apply all of the folds and draw the final image.
(draw-points "data/output.png" (apply-folds points folds))
