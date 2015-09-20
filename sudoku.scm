(require-extension srfi-1)
(require-extension srfi-13)
(require-extension srfi-69)
(require-extension traversal)
(require-extension json)

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
  (let* ([old-value (vector-ref gr ix)]
         [new-value (string-delete vl old-value)]
         [new-length (string-length new-value)])
         (if (= (string-length old-value) new-length)
            #t ; already eliminated
            (begin
              ; assign the new value
              (vector-set! gr ix new-value)
              ; propagate new constraints implied by that value and
              ; return #f if the constraints are impossible
              (let ([ok (cond [(zero? new-length) #f]
                              [(= new-length 1)
                                  (all?
                                      (lambda (x) (eliminate gr x (string-ref new-value 0)))
                                      (vector-ref peers ix))]
                              [else #t])])
                (if ok
                  (all?
                    (lambda (u)
                      (let ([places (filter (lambda (s) (string-index (vector-ref gr s) vl)) u)])
                        (cond [(zero? (length places)) #f]
                              [(= (length places) 1) (assign gr (car places) vl)]
                              [else #t])))
                    (vector-ref units ix))
                    #f))))))

(define (assign gr ix vl)
  (let ([old (string-delete vl (vector-ref gr ix))])
    (all?
      (lambda (vl) (eliminate gr ix vl))
      (string->list old))))

(define (reduce-vector-indexed f i c)
  (letrec
    ([
      reduce (lambda (f i c ix n)
                  (if (< ix n)
                    (reduce f (f i (vector-ref c ix) ix) c (add1 ix) n)
                    i))
    ])
    (reduce f i c 0 (vector-length c))))

(define (map-find f p l)
  (if (null? l)
    #f
    (let ([v (f (car l))])
      (if (p (car l) v)
        v
        (map-find f p (cdr l))))))

(define (search gr)
  (let ([min-unsolved
        (lambda (gr)
          (reduce-vector-indexed
            (lambda (c x i)
              (let ([n (string-length x)])
                (cond [(= n 1) c]
                      [(not c) (cons i n)]
                      [(< n (cdr c)) (cons i n)]
                      [else c])))
              #f
              gr))])
            (if (all?
                (lambda (s) (= (string-length (vector-ref gr s)) 1))
                squares)
                gr
                (let ([min-ix (car (min-unsolved gr))])
                  (map-find
                    (lambda (a)
                      (let ([ngr (vector-copy gr)])
                        (if (assign ngr min-ix a)
                          (search ngr)
                          #f)))
                    (lambda (a v) (not (eq? v #f)))
                    (string->list (vector-ref gr min-ix)))
                  ))))

(define (string-delete-all str xxx)
  (foldl
    (lambda (c s) (string-delete s c))
    str
    (string->list xxx)))

; TODO(knorton): This shit has to exist. I just can't find it.
(define (all? lmb lst)
  (if (null? lst)
    #t
    (if (lmb (car lst))
        (all? lmb (cdr lst))
        #f)))

(define (vector-copy ov)
  (let ([nv (make-vector (vector-length ov))])
    (begin
      (vector-copy! ov nv)
      nv)))

(define (parse-grid g)
  (let
    (
      [gl (map
          (lambda (x) (if (string-index all-values x) x #f))
          (string->list g))]
      [gr (make-grid)]
    )
    (if (not (= (length gl) 81))
      #f
      (begin
        (for-each-indexed
          (lambda (x ix) (if x (assign gr ix x) #f))
          gl)
        gr))))

(define (vector->hash-table v)
  (let ([ht (make-hash-table)])
    (begin
      (for-each-vector
        (lambda (p) (hash-table-set! ht (car p) (cdr p)))
        v))))

(define example-grid-1 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(define example-grid-2 ".26.39......6....19.....7.......4..9.5....2....85.....3..2..9..4....762.........4")

;(call-with-input-file "parse-grid.txt"
;  (lambda (p)
;    (define data (json-read p))
;    (for-each
;      (lambda (puz)
;        (define ht (vector->hash-table puz))
;        (define g (hash-table-ref ht "g"))
;        (define gr (parse-grid g))
;        (define ex (list->vector (hash-table-ref ht "s")))
;        (if (not (equal? gr ex))
;          (display (format "~A wrong.\n Expected: ~A\n Got: ~A\n\n" g ex gr))))
;      data)))

(for-each
  (lambda (g)
    (display (format "~A\n"
      (search (parse-grid g)))))
  (list example-grid-1 example-grid-2))
