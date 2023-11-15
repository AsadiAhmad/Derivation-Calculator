#lang racket
(define (plus a b)
  (list'+ a b)
)
(define (mines a b)
  (list'- a b)
)
(define (multi a b)
  (list'* a b)
)
(define (pow a b)
  (list'^ a b)
)

(define (ds x var)
  (cond
    [(number? x) 0]
    [(symbol? x)
      (if (equal? x var) 1 0)
    ]

    [(equal? (first x) '^)
      (define a 0)
      (define b 0)
      (define c 0)
      (set! a (list-ref x 1))
      (set! b (list-ref x 2))
      (set! c (ds a var))
      (multi b (multi (pow a (mines b 1)) c))
    ]
    [(equal? (first x) '*)
      (define a 0)
      (define b 0)
      (define c 0)
      (define d 0)
      (set! a (list-ref x 1))
      (set! b (list-ref x 2))
      (set! c (ds a var))
      (set! d (ds b var))
      (plus (multi a d) (multi c b))
    ]
    [(equal? (first x) '+)
      (define a 0)
      (define b 0)
      (define c 0)
      (define d 0)
      (set! a (list-ref x 1))
      (set! b (list-ref x 2))
      (set! c (ds a var))
      (set! d (ds b var))
      (plus c d)
    ]
    [(equal? (first x) '-)
      (define a 0)
      (define b 0)
      (define c 0)
      (define d 0)
      (set! a (list-ref x 1))
      (set! b (list-ref x 2))
      (set! c (ds a var))
      (set! d (ds b var))
      (mines c d)
    ]
    
    [(and (number? (first x)) (equal? (length x) 1)) 0]
    [(and (symbol? (first x)) (equal? (length x) 1))
      (if (equal? (first x) var) 1 0)
    ]
    
    [else error "d is wrong"]
  )
)

(define (simpPlus x)
  (cond
    [(and (number? (list-ref x 1)) (number? (list-ref x 2)))
      (+ (list-ref x 1) (list-ref x 2))
    ]
    [(equal? (list-ref x 1) 0)
      (simp (list-ref x 2))
    ]
    [(equal? (list-ref x 2) 0)
      (simp (list-ref x 1))
    ]
    [(and (number? (simp (list-ref x 1))) (number? (simp (list-ref x 2))))
      (+ (simp (list-ref x 1)) (simp (list-ref x 2)))
    ]
    [(equal? (simp (list-ref x 1)) 0)
      (simp (list-ref x 2))
    ]
    [(equal? (simp (list-ref x 2)) 0)
      (simp (list-ref x 1))
    ]
    [else (list'+ (simp (list-ref x 1)) (simp (list-ref x 2)))]
  )
)

(define (simpMulti x)
  (cond
    [(and (number? (list-ref x 1)) (number? (list-ref x 2)))
      (* (list-ref x 1) (list-ref x 2))
    ]
    [(equal? (list-ref x 1) 0)
      0
    ]
    [(equal? (list-ref x 2) 0)
      0
    ]
    [(equal? (list-ref x 1) 1)
      (simp (list-ref x 2))
    ]
    [(equal? (list-ref x 2) 1)
      (simp (list-ref x 1))
    ]
    [(and (number? (simp (list-ref x 1))) (number? (simp(list-ref x 2))))
      (* (simp(list-ref x 1)) (simp(list-ref x 2)))
    ]
    [(equal? (simp (list-ref x 1)) 0)
      0
    ]
    [(equal? (simp (list-ref x 2)) 0)
      0
    ]
    [(equal? (simp (list-ref x 1)) 1)
      (simp (list-ref x 2))
    ]
    [(equal? (simp (list-ref x 2)) 1)
      (simp (list-ref x 1))
    ]
    [else (list'* (simp (list-ref x 1)) (simp (list-ref x 2)))]
  )
)

(define (simpPow x)
  (cond
    [(and (number? (list-ref x 1)) (number? (list-ref x 2)))
      (expt (list-ref x 1) (list-ref x 2))
    ]
    [(equal? (list-ref x 1) 0)
      0
    ]
    [(equal? (list-ref x 1) 1)
      1
    ]
    [(equal? (list-ref x 2) 0)
      1
    ]
    [(equal? (list-ref x 2) 1)
      (simp (list-ref x 1))
    ]
    [(and (number? (simp (list-ref x 1))) (number? (simp (list-ref x 2))))
      (expt (simp (list-ref x 1)) (simp (list-ref x 2)))
    ]
    [(equal? (simp (list-ref x 1)) 0)
      0
    ]
    [(equal? (simp (list-ref x 1)) 1)
      1
    ]
    [(equal? (simp (list-ref x 2)) 0)
      1
    ]
    [(equal? (simp (list-ref x 2)) 1)
      (simp (list-ref x 1))
    ]
    [else (list'^ (simp (list-ref x 1)) (simp (list-ref x 2)))]
  )
)

(define (simpMines x)
  (cond
    [(and (number? (list-ref x 1)) (number? (list-ref x 2)))
      (- (list-ref x 1) (list-ref x 2))
    ]
    [(equal? (list-ref x 2) 0)
      (simp (list-ref x 1))
    ]
    [(and (simp (number? (list-ref x 1))) (simp (number? (list-ref x 2))))
      (- (list-ref x 1) (list-ref x 2))
    ]
    [(equal? (simp (list-ref x 2)) 0)
      (simp (list-ref x 1))
    ]
    
    [else (list'^ (simp (list-ref x 1)) (simp (list-ref x 2)))]
  )
)

(define (simp x)
  (cond
    [(number? x) x]
    [(symbol? x) x]
    [(equal? (first x) '+)
      (simpPlus x)
    ]
    [(equal? (first x) '-)
      (simpMines x)
    ]
    [(equal? (first x) '*)
      (simpMulti x)
    ]
    [(equal? (first x) '^)
      (simpPow x)
    ]
    [else error "exp is wrong"]
  )
)
(ds '(* (* (* x x) x) 3)'x)
(simp (ds '(* (* (* x x) x) 3)'x))
(ds '(* y (* 8 5))'x)
(simp (ds '(* y (* 8 5))'x))
(ds '(^ x 1)'x)
(simp (ds '(^ x 1)'x))
