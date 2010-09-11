(assert-equal
	6
	(.asm add Int32
		(.asm (ldc.i4 2) Int32) 
		(.asm (ldc.i4 4) Int32)))
