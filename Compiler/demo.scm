(.using System)
(define (countTo total acc)
  (if (= total acc)
    acc
    (countTo total (+ 1 acc))))
(define (factorial n acc)
  (if (= n 0)
    acc
    (factorial (- n 1) (* acc n))))
(define number 6)
(define (display-int n)
        (.asm (call Console.WriteLine Int32) Void n))
(display-int (factorial (countTo number 0) 1))
