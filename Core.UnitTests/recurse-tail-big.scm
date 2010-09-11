(define (countTo total acc)
    (if (= total acc)
	    acc
	    (countTo total (+ 1 acc))))
(assert-equal 10000000 (countTo 10000000 0))
