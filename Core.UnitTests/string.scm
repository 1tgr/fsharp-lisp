(define (assert-equal-string (expected String) (actual String))
        (.asm (call Assert.Equal String String) Void expected actual))
(assert-equal-string "hello" "hello")
