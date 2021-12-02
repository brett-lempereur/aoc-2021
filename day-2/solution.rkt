#lang racket

(require brag/support)
(require syntax/parse)

(require "parser.rkt")

;; A position structure.
(struct position (distance depth aim) #:transparent)

;; A lexer for a navigation script.
(define navigation-lexer
  (lexer-srcloc
   [(:or "forward" "up" "down") (token lexeme lexeme)]
   [(repetition 1 +inf.0 numeric) (token 'INTEGER (string->number lexeme))]
   [whitespace (token lexeme #:skip? #t)]
   [(eof) (void)]))

;; A tokenizer for a navigation script.
(define (make-navigation-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token) (navigation-lexer ip))
  next-token)

;; Navigation command function builds.
(define (make-forward amount)
  (lambda (pos)
    (position (+ (position-distance pos) amount)
              (+ (position-depth pos) (* (position-aim pos) amount))
              (position-aim pos))))
(define (make-up amount)
  (lambda (pos)
    (position (position-distance pos)
              (position-depth pos)
              (- (position-aim pos) amount))))
(define (make-down amount)
  (lambda (pos)
    (position (position-distance pos)
              (position-depth pos)
              (+ (position-aim pos) amount))))

;; Transforms a navigation route into a sequence of navigation command
;; functions.
(define (nav-route->commands nav-route-stx)
  (syntax-parse nav-route-stx
    [({~literal nav-route} commands ...)
     (for/list ([nav-command-stx (syntax->list #'(commands ...))])
       (syntax-parse nav-command-stx
         [({~literal nav-command}
           ({~literal nav-verb} "forward") ({~literal nav-amount} amount))
          (make-forward (syntax-e #'amount))]
         [({~literal nav-command}
           ({~literal nav-verb} "up") ({~literal nav-amount} amount))
          (make-up (syntax-e #'amount))]
         [({~literal nav-command}
           ({~literal nav-verb} "down") ({~literal nav-amount} amount))
          (make-down (syntax-e #'amount))]))]))

;; Traverse a navigation route and return the final position.
(define (traverse commands initial-position)
  (foldl (lambda (c p) (c p)) initial-position commands))

;; Returns the index of a position for the AoC form.
(define (position->index pos)
  (* (position-distance pos) (position-depth pos)))

;; Load and parse the file.
(define input-filename "data/input.txt")
(define input-stx
  (parse
   (make-navigation-tokenizer
    (open-input-file input-filename)
    input-filename)))
(define commands (nav-route->commands input-stx))

;; Apply the commands to an initial position
(define final-position
  (traverse commands (position 0 0 0)))
(position->index final-position)
