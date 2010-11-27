(define (factorial n)
    (if (= n 0) 
    1 
    (* n (factorial (- n 1)))))
(assert-equal 720 (factorial 6))
