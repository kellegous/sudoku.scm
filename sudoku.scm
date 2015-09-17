(require-extension srfi-1)
(require-extension srfi-13)
(require-extension srfi-69)
(require-extension traversal)

; Produce a list of integers from a to b exclusive, taking
; steps of s (default 1).
(define (range a b . s)
  (let ([s (optional s 1)])
    (if (>= a b)
      '()
      (cons a (range (+ s a) b s)))))

(define (repeat n v)
  (if (= n 0)
  '()
  (cons v (repeat (sub1 n) v))))

(define all-values "123456789")

; all square indices
(define squares (range 0 81))

; A list of all 27 units of constraint. A unit of constraint is a
; group of positions to which each of the values "123456789" must
; be assigned uniquely.
(define all-units
  (append
    ; rows
    (map
      (lambda (x) (range (* x 9) (* (add1 x) 9)))
      (range 0 9))

    ; cols
    (map
      (lambda (x) (range x 81 9))
      (range 0 9))

    ; squares
    (map
      (lambda (i)
        (map
          (lambda (j)
            (+ (modulo j 3)
               (* (modulo i 3) 3)
               (* (quotient j 3) 9)
               (* (quotient i 3) 27)))
          (range 0 9)))
      (range 0 9))
  )
)

; maps a square to all the units it's a member of
(define units
  (list->vector
    (map
      (lambda (s) (filter
                    (lambda (u) (member s u))
                    all-units))
      squares)))

; maps a square to the index of each square to which it is constrained.
(define peers
  (map-indexed-vector
    (lambda (x i) (cdr (delete-duplicates
                    (cons i (apply append x)))))
    units))

; Create a vector representing a sudoku board with no assignments.
(define (make-grid)
  (list->vector (repeat 81 all-values)))

(define (eliminate gr ix vl)
  (define old-value (vector-ref gr ix))
  (define new-value (string-delete vl old-value))
  (define new-length (string-length new-value))
  (if (= (string-length old-value) new-length)
      #t ; already eliminated
      (begin
        (vector-set! gr ix new-value)
        (define ok
          (cond [(zero? new-length) #f]
                [(= new-length 1) (all?
                                    (lambda (x) (eliminate gr x (string-ref new-value 0)))
                                    (vector-ref peers ix))]
                [else #t]))
        (if ok
          (all?
            (lambda (u)
              (define places
                (filter
                  (lambda (s) (string-index (vector-ref gr s) vl))
                  u))
              (cond [(zero? (length places)) #f]
                    [(= (length places))
                      (assign gr (car places) vl)]
                    [else #t]))
            (vector-ref units ix))
          #f)
      )))

(define (assign gr ix vl)
  (define old (string-delete vl (vector-ref gr ix)))
  ; (vector-set! gr ix (make-string 1 vl))
  (all?
    (lambda (vl) (eliminate gr ix vl))
    (string->list old)))

(define (string-delete-all str xxx)
  (foldl
    (lambda (c s) (string-delete s c))
    str
    (string->list xxx)))

(define (all? lmb lst)
  (if (null? lst)
    #t
    (if (lmb (car lst))
        (all? lmb (cdr lst))
        #f)))

(define (for-each-with-index lmb lst)
  (define (nop a b) '())
  (define (each ix lmb lst)
    (if (null? lst)
      '()
      (nop (lmb (car lst) ix) (each (add1 ix) lmb (cdr lst)))))
  (each 0 lmb lst))

(define (parse-grid g)
  (define gl
    (map
        (lambda (x) (if (string-index all-values x) x #f))
        (string->list g)))
  (define gr (make-grid))
  (if (not (= (length gl) 81))
    #f
    (begin
      (for-each-with-index
        (lambda (x ix) (if x (assign gr ix x) #f))
        gl)
      gr)))

(define example-grid "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

(display (parse-grid example-grid))
(newline)
