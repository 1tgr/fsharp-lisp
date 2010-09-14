(defmacro say-hello
          (.asm (call Console.WriteLine String) Void "hello world"))
(say-hello)
(say-hello)
(say-hello)