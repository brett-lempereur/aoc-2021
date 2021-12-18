#lang racket/base

(require racket/list)
(require racket/match)
(require racket/math)
(require racket/sequence)

;;;
;;; Parsing
;;;

;; Get the current namespace so that we can eval the expressions.
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

;; Interpret a file as a list of expressions.
(define (file->expressions filename)
  (sequence-map
   (lambda (l) (eval (read (open-input-string l)) ns))
   (in-lines (open-input-file filename))))

;; Recursively visit all terms in an expression and return a list of
;; depth-term pairs.
(define (expression->depth-list expression [depth 0])
  (if
   (list? expression)
   (append (expression->depth-list (first expression) (add1 depth))
           (expression->depth-list (second expression) (add1 depth)))
   (list (cons depth expression))))

;;;
;;; Evaluation
;;;

;; The depth at which pairs explode.
(define explode-depth 5)

;; The threshold at which values split.
(define split-threshold 10)

;; Reduce a set of terms until they are stable.
(define (reduce terms)
  (let ([exploded (explode terms)])
    (if
     (equal? exploded terms)
     (let ([splitted (split terms)])
       (if (equal? splitted terms)
           terms
           (reduce splitted)))
     (reduce exploded))))

;; Explode the first viable number in a depth list.
(define (explode terms)
  (let ([i (index-where terms (lambda (t) (>= (car t) explode-depth)))])
    (if i (explode-at-index terms i) terms)))

;; Explode the pair that starts at the given index and return the
;; updated list of term-depth pairs.
(define (explode-at-index terms i)
  (let* ([u (list-ref terms i)]
         [v (list-ref terms (add1 i))]
         [t (add-to-term (add-to-term terms (- i 1) u) (+ i 2) v)])
    (append (take t i) (list (cons (sub1 explode-depth) 0)) (drop t (+ i 2)))))

;; Add the given value to the term at position j.
(define (add-to-term terms j v)
  (if (or (< j 0) (>= j (length terms)))
      terms
      (list-update terms j (lambda (t) (cons (car t) (+ (cdr v) (cdr t)))))))

;; Split the first viable number in a depth list.
(define (split terms)
  (let ([i (index-where terms (lambda (t) (>= (cdr t) split-threshold)))])
    (if i (split-at-index terms i) terms)))

;; Split the number at the given index.
(define (split-at-index terms i)
  (let* [(term (list-ref terms i))
         [depth (add1 (car term))]
         [value (cdr term)]]
    (append (take terms i)
            (list (cons depth (exact-floor (/ value 2)))
                  (cons depth (exact-ceiling (/ value 2))))
            (drop terms (add1 i)))))

;; Join two depth lists into an addition.
(define (add left right)
  (map
   (lambda (t) (cons (add1 (car t)) (cdr t)))
   (append left right)))

;; Compute the reduced sum of a list of depth-lists.
(define (sum depth-lists)
  (foldl
   (lambda (dl acc)
     (reduce (add acc dl)))
   (first depth-lists)
   (rest depth-lists)))

;; Compute the magnitude of a depth-list.
(define (magnitude depth-list)
  (let loop ([terms depth-list] [depth 4] [p null] [out (list)])
    (cond
      [(<= depth 0) (cdar terms)]
      [(empty? terms) (loop out (sub1 depth) null (list))]
      [else
       (match-let ([(cons d v) (first terms)] [tail (rest terms)])
         (cond
           [(and (= depth d) (null? p)) (loop tail depth v out)]
           [(= depth d)
            (loop
             tail
             depth
             null
             (append out (list (cons (sub1 depth) (+ (* 3 p) (* 2 v))))))]
           [else (loop tail depth null (append out (list (cons d v))))]))])))

;;;
;;; Solution
;;;

;; Load the expressions, convert them to depth-lists, and reduce them.
(define expressions
  (sequence->list (file->expressions "data/input-prepared.txt")))
(define depth-lists
  (map reduce (map expression->depth-list expressions)))

;; Compute and display the solutions.
(define solution-a (magnitude (sum depth-lists)))
(define solution-b
  (exact-floor
   (foldl
    max
    -inf.0
    (for*/list ([a depth-lists] [b depth-lists])
      (if (equal? a b) -inf.0 (magnitude (reduce (add a b))))))))
(printf "The magnitude of the sum of all expressions: ~a\n" solution-a)
(printf "The largest magnitude of all expression pairs: ~a\n" solution-b)
