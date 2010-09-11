(define (factorial n acc)
    (if (= n 0)
    acc
    (factorial (- n 1) (* acc n))))
(assert-equal 720 (factorial 6 1))
