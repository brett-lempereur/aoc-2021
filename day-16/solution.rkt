#lang racket/base

(require racket/format)
(require racket/function)
(require racket/list)
(require racket/sequence)

;;;
;;; Data structures
;;;

;; A literal value packet.
(struct literal (version id value) #:transparent)

;; An operator packet.
(struct operator (version id packets) #:transparent)

;;;
;;; Parsing
;;;

;; Load a port and convert it to a binary representation.
(define (port->binary port)
  (flatten
   (sequence->list
    (sequence-map
     (lambda (s)
       (map (lambda (c) (string->number (string c) 2)) (string->list s)))
     (sequence-map
      (lambda (c)
        (~r (string->number (string c) 16)
            #:base 2 #:min-width 4 #:pad-string "0"))
      (sequence-filter
       (compose not (curry equal? #\newline))
       (in-input-port-chars port)))))))

;; Load an input and convert it to a binary representation.
(define (file->binary filename)
  (port->binary (open-input-file filename)))

;; Load a string input.
(define (string->binary input)
  (port->binary (open-input-string input)))

;; Decode an input.
(define (decode binary)
  (let-values ([(packet _) (decode-packet binary)])
    packet))

;; Decode the next packet.
(define (decode-packet binary)
  (let-values ([(version id) (decode-header binary)])
    (case id
      [(4) (decode-literal-packet binary)]
      [else (decode-operator-packet binary)])))

;; Return the next packet version and type from a binary sequence.
(define (decode-header binary)
  (values
   (binary->number (take binary 3))
   (binary->number (take (drop binary 3) 3))))

;; Decode a literal packet.
(define (decode-literal-packet binary)
  (let-values ([(version id) (decode-header binary)]
               [(payload rb) (decode-literal-packet-payload (drop binary 6))])
    (values (literal version id payload) rb)))

;; Decode an operator packet.
(define (decode-operator-packet binary)
  (let-values ([(version id) (decode-header binary)]
               [(encoding) (list-ref binary 6)])
    (let-values
        ([(payload rb)
          (if
           (eq? encoding 0)
           (decode-length-delimited-operator-payload (drop binary 7))
           (decode-count-delimited-operator-payload (drop binary 7)))])
      (values (operator version id payload) rb))))

;; Decode a length-delimited operator payload.
(define (decode-length-delimited-operator-payload binary)
  (let* ([len (binary->number (take binary 15))]
         [payload (drop binary 15)])
    (let loop ([proc (take payload len)] [acc (list)])
      (if
       (empty? proc)
       (values acc (drop payload len))
       (let-values ([(pkt rem) (decode-packet proc)])
         (loop rem (append acc (list pkt))))))))

;; Decode a fixed-number operator payload.
(define (decode-count-delimited-operator-payload binary)
  (let* ([cnt (binary->number (take binary 11))]
         [payload (drop binary 11)])
    (let loop ([proc payload] [acc (list)] [n cnt])
      (if
       (<= n 0)
       (values acc proc)
       (let-values ([(pkt rem) (decode-packet proc)])
         (loop rem (append acc (list pkt)) (sub1 n)))))))

;; Decode the payload of a literal packet.
(define (decode-literal-packet-payload binary)
  (let loop ([acc 0] [seq binary])
    (let* ([flag (first seq)]
           [n (binary->number (take (drop seq 1) 4))]
           [nacc (bitwise-ior (arithmetic-shift acc 4) n)])
      (case flag
        [(0) (values nacc (drop seq 5))]
        [(1) (loop nacc (drop seq 5))]))))

;; Given a binary list produces its number representation.
(define (binary->number binary)
  (foldl (lambda (n acc) (bitwise-ior (arithmetic-shift acc 1) n)) 0 binary))

;;;
;;; Processing
;;;

;; Compute the sum of the versions of nested packets.
(define (sum-versions packet)
  (cond
    [(literal? packet) (literal-version packet)]
    [(operator? packet)
     (foldl
      +
      (operator-version packet)
      (map sum-versions (operator-packets packet)))]
    [else (error "unexpected packet type")]))

;; Evaluate a packet.
(define (evaluate packet)
  (cond
    [(literal? packet) (literal-value packet)]
    [(operator? packet)
     (case (operator-id packet)
       [(0) (foldl + 0 (map evaluate (operator-packets packet)))]
       [(1) (foldl * 1 (map evaluate (operator-packets packet)))]
       [(2) (foldl min +inf.0 (map evaluate (operator-packets packet)))]
       [(3) (foldl max -inf.0 (map evaluate (operator-packets packet)))]
       [(5) (if (apply > (map evaluate (operator-packets packet))) 1 0)]
       [(6) (if (apply < (map evaluate (operator-packets packet))) 1 0)]
       [(7) (if (apply = (map evaluate (operator-packets packet))) 1 0)]
       [else (error "unexpected operator id")])]
    [else (error "unexpected packet type")]))

;;;
;;; Solution
;;;

;; Load the input file.
(define input-binary (file->binary "data/input.txt"))

(sum-versions (decode input-binary))
(evaluate (decode input-binary))
