#lang racket/base

(require racket/function)
(require racket/generator)
(require racket/list)
(require racket/match)
(require racket/sequence)

;;;
;;; Images
;;;

;; An image.
(struct image (bits p q r s))

;; Enlarge the width and height of an image by the given number of
;; pixels.
(define (image-enlarge img n)
  (match-let ([(image bits p q r s) img])
    (image bits (- p n) (- q n) (+ r n) (+ s n))))

;; Return the value of a pixel in an image.
(define (image-pixel img x y d)
  (hash-ref (image-bits img) (list x y) d))

;; Return the reference index of a pixel in an image.
(define (image-index img x y d)
  (for*/fold [(acc 0)]
             [(uy (list (sub1 y) y (add1 y))) (ux (list (sub1 x) x (add1 x)))]
    (bitwise-ior (arithmetic-shift acc 1) (if (image-pixel img ux uy d) 1 0))))

;; Return a generator that yields all pixel indices of the image.
(define (image-indices img d)
  (match-let ([(image bits p q r s) img])
    (in-generator
     (for* [(y (range q s)) (x (range q r))]
       (yield (list (image-index img x y d) x y))))))

;; Print the pixels of an image.
(define (image-print img)
  (match-let ([(image bits p q r s) img])
    (for [(y (range q s))]
      (for [(x (range p r))]
        (printf (if (image-pixel img x y) "#" ".")))
      (printf "\n"))))

;; Return the number of enabled pixels in the image.
(define (image-count-set img)
  (count (curry eq? #t) (hash-values (image-bits img))))

;;;
;;; Parsing
;;;

;; Load the reference values from an input port.
(define (port->reference port)
  (let ([lines (sequence->list (in-lines port))])
    (vector->immutable-vector
     (list->vector
      (map symbol->boolean (string->list (first lines)))))))

;; Load the image from an input port.
(define (port->image port)
  (let* ([lines (drop (sequence->list (in-lines port)) 2)]
         [input (map symbol->boolean (flatten (map string->list lines)))]
         [width (string-length (first lines))]
         [height (length lines)])
    (let loop [(acc (hash)) (input input) (x 0) (y 0)]
      (if (empty? input)
          (image acc 0 0 width height)
          (loop (hash-set acc (list x y) (first input))
                (rest input)
                (if (= x (sub1 width)) 0 (add1 x))
                (if (= x (sub1 width)) (add1 y) y))))))

;; Convert an input symbol to a boolean value.
(define (symbol->boolean sym)
  (eq? sym #\#))

;;;
;;; Enhancement
;;;

;; Enhance an image using a reference table.
(define (enhance img ref d)
  (image
   (sequence-fold
    (lambda (acc p)
      (match-let ([(list i x y) p])
        (hash-set acc (list x y) (vector-ref ref i))))
    (hash)
    (image-indices img d))
   (image-p img)
   (image-q img)
   (image-r img)
   (image-s img)))

;; Repeatedly enhance an image a given number of steps.
(define (repeat-enhance img ref n)
  (if
   (<= n 0)
   img
   (repeat-enhance
    (enhance (image-enlarge img 2) ref (if (even? n) #f #t)) ref (sub1 n))))

;;;
;;; Solution
;;;

(define input-reference (port->reference (open-input-file "data/input.txt")))
(define input-image (port->image (open-input-file "data/input.txt")))

(image-count-set (repeat-enhance input-image input-reference 2))
(image-count-set (repeat-enhance input-image input-reference 50))
