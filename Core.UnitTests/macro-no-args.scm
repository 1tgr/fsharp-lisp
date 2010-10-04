(defmacro say-hello
          (.asm (call Console.Write String) Void "hello ")
		  (.asm (call Console.WriteLine String) Void "world"))
(say-hello)
(say-hello)
(say-hello)