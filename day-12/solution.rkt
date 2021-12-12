#lang racket/base

(require racket/function)
(require racket/list)
(require racket/match)
(require racket/set)
(require racket/sequence)
(require racket/string)

;; An undirected graph edge.
(struct edge (u v) #:transparent)

;;;
;;; Parsing
;;;

;; Interpret a graph file as an edge sequence.
(define (file->edge-sequence filename)
  (let ([input (in-lines (open-input-file filename))])
    (sequence-map
     (lambda (l)
       (match-let ([(list u v) (string-split l "-")])
         (edge u v)))
     input)))

;; Add an edge to an adjacency list.
(define (add-edge a e)
  (hash-update a (edge-u e) (lambda (s) (set-add s (edge-v e))) (set)))

;; Reverse an edge.
(define (reverse-edge e)
  (edge (edge-v e) (edge-u e)))

;; Convert an edge sequence to an adjacency list.
(define (edge-sequence->adjacency-list edges)
  (sequence-fold
   (lambda (acc e)
     (add-edge (add-edge acc e) (reverse-edge e)))
   (hash)
   edges))

;;;
;;; Vertex properties
;;;

;; Holds if a vertex is the start room.
(define (is-start? v) (equal? v "start"))

;; Holds if a vertex is the end room.
(define (is-end? v) (equal? v "end"))

;; Holds if a vertex represents a large room.
(define (is-large? v) (andmap char-upper-case? (string->list v)))

;;;
;;; Traversal
;;;

;; Return the list of vertices adjacent to the given vertex.
(define (adjacent a n) (set->list (hash-ref a n (list))))

;; Holds if the given cave can be visited where a small cave can be
;; visited only once, and a large cave an unlimited number of times.
(define (can-visit? n p)
  (or (is-large? n)
      (not (ormap (curry equal? n) p))))

;; Holds if the given cave can be visited.
(define (can-visit-optimistic? n p)
  (let ([c (count (curry equal? n) p)])
    (cond
      [(is-large? n) #t]
      [(is-start? n) (= c 0)]
      [(is-end? n) (= c 0)]
      [else (or (= c 0) (not (visited-small-cave-twice? p)))])))

;; Holds if a small cave has been visited twice on a path.
(define (visited-small-cave-twice? p)
  (let ([fp
         (filter-not
          (lambda (q) (or (is-large? q) (is-start? q) (is-end? q)))
          p)])
    (ormap
     (lambda (n) (> n 1))
     (hash-values
      (foldl
       (lambda (q acc) (hash-update acc q add1 0))
       (hash)
       fp)))))

(define (search-until a c [n "start"] [f "end"] [p null])
  (let ([np (append p (list n))])
    (if
     (equal? n f) 1
     (let ([ts (filter (lambda (q) (c q np)) (adjacent a n))])
       (if (empty? ts) 0
           (foldl + 0
                  (map
                   (lambda (q) (search-until a c q f np))
                   ts)))))))

;;;
;;; Solutions
;;;

;; Load the adjaency list.
(define adjacency-list
  (edge-sequence->adjacency-list
   (file->edge-sequence "data/input.txt")))

;; Find and print both solutions.
(define start-to-end-paths
  (search-until adjacency-list can-visit?))
(define start-to-end-optimistic-paths
  (search-until adjacency-list can-visit-optimistic?))
(printf "Number of paths under pessimistic constraints: ~a\n"
        start-to-end-paths)
(printf "Number of paths under optimistic constraints: ~a\n"
        start-to-end-optimistic-paths)
