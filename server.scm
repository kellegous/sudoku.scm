(require-extension fastcgi)
(require-extension uri-common)
(require-extension medea)
(require-extension srfi-19)

(declare (uses sudoku))

(define (get-grid env def)
  (let ([
    grid (assoc 'grid
            (uri-query (uri-reference (env "REQUEST_URI"))))
  ])
  (if grid (cdr grid) def)))

(define (emit-err out tm)
  (out (json->string
    (list
      (cons 'err #t)
      (cons 'dur (timer-elapsed tm))))))

(define (emit-res out sln tm)
  (out
    (json->string
      (list
        (cons 'err #f)
        (cons 'sln (string-join (vector->list sln)  ""))
        (cons 'dur (timer-elapsed tm))))))

(define (emit-json out sln tm)
  (if sln
    (emit-res out sln tm)
    (emit-err out tm)))

(define (make-timer)
  (current-time))

(define (timer-elapsed t)
  (/ (- (time-nanosecond (current-time)) (time-nanosecond t)) 1000000))

(fcgi-accept-loop
  9000
  0
  (lambda (in out err env)
    (let* (
        [ gs (get-grid env #f) ]
        [ gg (parse-grid gs) ]
        [ tm (make-timer)])
      (out "Content-Type: application/json;charset=utf-8\r\n\r\n")
      (if (not gg)
        (emit-err out tm)
        (emit-json out (search gg) tm)))))
