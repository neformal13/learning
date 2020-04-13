(define (square x) (* x x))
(define (average x y) 
  (/ (+ x y) 2))

(define (improve guess x) 
  (average guess (/ x guess)))

(define (good-enough? guess x) 
  (< (abs (- (square guess) x)) 0.001))

(define (better-good-enough? guess prev-guess) 
  (< (abs (/ (- guess prev-guess) prev-guess)) 0.001))


(define (better-sqrt-iter guess prev-guess x) 
  (if (better-good-enough? guess prev-guess) 
      guess 
      (better-sqrt-iter (improve guess x) 
                        guess 
                        x)))

(define (sqrt-iter guess x) 
  (if (good-enough? guess x) 
      guess 
      (sqrt-iter (improve guess x) 
                 x)))

(define (sqrt x) 
  (sqrt-iter 1.0 x))

(define (better-sqrt x) 
  (better-sqrt-iter 1.0 0.5 x))

(define (root-3-iter guess prev-guess x)
  (if (better-good-enough? guess prev-guess) 
    guess 
    (better-sqrt-iter (improve-root-3 guess x) 
                      guess 
                      x)))
)

(define (root-3 x)
  (root-3-iter 1.0 0.5 x)
)
