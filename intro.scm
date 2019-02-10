(define (dec x)
  (- x 1))

(define (inc x) (+ 1 x))

(define (identity x) x)

(define (square n) (* n n))

;; Ex. 1.3
(define sum-2-largest-squares
  (lambda (a b c)
    (let* ([a>b (>= a b)]
           [b>c (>= b c)]
           [a>c (>= a c)]
           [pair (cond ((and a>b b>c) (list a b))
                       ((and b>c a>c) (list a b))
                       ((and (not a>b) b>c) (list b c))
                       ((and (not b>c) a>c) (list a c))
                       ((and (not a>b) (not b>c)) (list b c))
                       ((and (not a>c) a>b) (list a c)))]

           [squared (map square pair)])
      (+ (car squared) (cadr squared)))))

;; All should eval to 13
(sum-2-largest-squares 1 2 3)
(sum-2-largest-squares 1 3 2)
(sum-2-largest-squares 2 1 3)
(sum-2-largest-squares 2 3 1)
(sum-2-largest-squares 3 1 2)
(sum-2-largest-squares 3 2 1)

;; testing equalities
(sum-2-largest-squares 2 2 2)
(sum-2-largest-squares 1 2 2)
(sum-2-largest-squares 2 1 2)
(sum-2-largest-squares 2 2 1)

;; ;; 1.1.7 Square Roots
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
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
  (sqrt-iter 1.0 x))

(sqrt 2)

;; ;; Ex. 1.8
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
(define (reduce fn l init)
  (if (null? l)
      init
      (reduce fn (cdr l) (fn init (car l)))))

(define (reverse l)
  (reduce (lambda (prev curr) (cons curr prev))
          l '()))

(define (cons-end x l)
  (reverse (cons x (reverse l))))

(cons-end 3 '(0 1 2))

(define (take-n n l)
  (define (first-n* n l result)
    (if (= n 0) result
        (first-n* (dec n) (cdr l) (cons (car l) result))))
  (reverse (first-n* n l '())))

(define (last-n n l)
  (reverse (take-n n (reverse l))))

(define (drop-n n l)
  (if (= n 0) l
      (drop-n (dec n) (cdr l))))


(drop-n 2 '(0 1 2 3))


(define (partition n l)
  (reverse (reduce (lambda (prev2 curr)
                     (let ([next (reverse
                                  (cons curr
                                        (take-n (dec n) (reverse (car prev2)))))])
                       (cons next prev2))) (drop-n n l) (list (take-n n l)))))

(define (make-row prev)
  (cons 1
        (cons-end 1
                  (map (lambda (l)
                             (+ (car l)
                                (cadr l)))
                           (partition 2 prev)))))

(make-row (list 1 2 2 1))



(define (pascals-triangle n)
  (define (pascals-triangle* n result)
    (cond ((= n 1)
           (cons (list 1) result))
          ((= n 2) (cons (list 1 1) (pascals-triangle* (dec n) result)))
          (else (let ([prev (pascals-triangle* (dec n) result)])
                  (cons (make-row (car prev))
                        prev)))))
  (reverse
   (pascals-triangle* n '())))

(pascals-triangle 5)


;; 1.16
(define (even? n)
  (= (remainder n 2)
     0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt (square b) (/ n 2)))
        (else (* b (fast-expt b (dec n))))))

(fast-expt 3 0)
(fast-expt 3 1) 3 24
(fast-expt 3 2) 9 18
(fast-expt 3 3) 27 0

;;         b n  a
(fast-expt 3 0) 81
(fast-expt 3 2) 9
(fast-expt 3 4) 1

(define (fast-expt-itr b n)
  (define (expt-itr* b n a)
    (cond ((= n 0) 1)
          ((even? n) (expt-itr* (square b) (/ n 2) (* a (square b))) )
          (else (* a (expt-itr* b (dec n) (* a b))))))

  (expt-itr* b n 1))


(fast-expt-itr 3 3)
(fast-expt-itr 3 100)


;; 1.17
(define (double n)
  (* 2 n))

(define (half n)
  (/ n 2))

(define (fast-multiply a b)
  (define (fast-multiply* a b)
    (cond ((or (= a 0) (= b 0)) 0)
          ((= a 1) b)
          ((= b 1) a)
          ((= b 2) (double a))
          ((= a 2) (double b))
          ((even? b) (fast-multiply (double a) (half b)))
          (else (+ a (fast-multiply a (dec b))))))
  ;; ensure number of recursions doesn't change if a < b
  (if (< a b)
      (fast-multiply* b a)
      (fast-multiply* a b)))

(fast-multiply 10 100)

;; ;; Zero's
(fast-multiply 0  5)
(fast-multiply 5  0)
;; multiplicative identity
(fast-multiply 1  5)
(fast-multiply 5  1)
                                        ; 3 * 4 = 3 + (3 * 3)

; a   b   p
; 6 * 4 = 24
; (double 2 6) * (half 4) = 24
; (doulbe 12) * (half 2) = 24; a = 1

;; 1.30


(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(sum identity 0 inc 3)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term  (next a) next  b))))

(product-recursive identity 1 inc 4)

(product identity 1 inc 4)
6 10 14

(define (pi accuracy)
  (define (next n)
    (/ (* (* 2 n) (+ 2 (* 2 n)))
       (square (+ (* 2 n) 1))))
  (* 4.0 (product next 1 inc accuracy)))

(pi 100)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next b))))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (let ([])
        (combiner (term a)
                  (accumulate combiner
                              null-value
                              term
                              (next a)
                              next b)))))

(accumulate + 0 identity 0 inc 3)

(define (sum-higher-order term a next b)
  (accumulate + 0 term a inc b))

(sum-higher-order identity 0 inc 3)

