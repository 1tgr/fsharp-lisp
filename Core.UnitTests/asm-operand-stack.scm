(define (char-upcase (c Char))
		(.asm (call Char.ToUpperInvariant Char) Char c))
(assert-equal #\X (char-upcase #\x))
