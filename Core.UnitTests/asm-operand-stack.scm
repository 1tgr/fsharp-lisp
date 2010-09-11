(define (char-upcase c)
		(.asm (call Char.ToUpperInvariant Char) Char c))
(assert-equal #\X (char-upcase #\x))
