(define (square n) (* n n))

;; Ex. 1.3
(define sum-2-largest-squares
  (lambda (a b c)
    (let* ([a>b (> a b)]
           [b>c (> b c)]
           [a>c (> a c)]
           [pair (cond ((and a>b b>c) (list a b))
                       ((and b>c a>c) (list a b))
                       ((and (not a>b) b>c) (list b c))
                       ((and (not b>c) a>c) (list a c))
                       ((and (not a>b) (not b>c)) (list b c))
                       (else 'error))]
           [squared (map square pair)])
      (+ (car squared) (cadr squared)))))

;; All should eval to 13
(sum-2-largest-squares 1 2 3)
(sum-2-largest-squares 1 3 2)
(sum-2-largest-squares 2 1 3)
(sum-2-largest-squares 2 3 1)
(sum-2-largest-squares 3 1 2)
(sum-2-largest-squares 3 2 1)

;; 1.1.7 Square Roots
(define (average x y)
  (/ (+ x y) 2))

(define (improve-sqrt guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .0000000001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-sqrt
                   guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2)

;; Ex. 1.8
(define (cube x) (* x x x))

(define (average3 x y z)
  (/ (+ x y z) 3))

(define (improve-cbrt guess x)
  (average3 (/ x (square guess)) guess guess))

(define (good-enough-cb? guess x)
  (< (abs (- (cube guess) x)) .0000000001))

(define (cbrt-iter guess x)
  (if (good-enough-cb? guess x)
      guess
      (cbrt-iter (improve-cbrt
                  guess x)
               x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 27)

;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((- x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

;; Ex. 1.1
(define (e1.11 n)
  (if (< n 3)
      n
      (+ (e1.11 (- n 1))
         (* 2 (e1.11 (- n 2)))
         (* (e1.11 (- n 3))))))

(e1.11 4)

;; Ex. 1.12
(define (pascals-triangle n)
  (cond ((= n 1) `(1))
        ((= n 2) (cons (pascals-triangle (- n 1)) `(1 1)))))
((= n 3) (cons (pascals-triangle (- n 1) )))



(define (reduce fn l init)
  (if (null? l)
      init
      (reduce fn (cdr l) (fn init (car l)))))

(define (reverse l)
  (reduce (lambda (prev curr) (cons curr prev))
          l '()))

(define (cons-end x l)
  (reduce (lambda (prev curr) (cons curr prev))
          (reverse l) (list x)))

(define (partition n l)
  ...)
(define (make-row prev)
  (map (lambda (l)
         (+ (car l))
         (cadr l))
       (partition 2 prev)))

(make-row (list 1 2 2 1))





