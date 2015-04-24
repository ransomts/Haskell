main = do
   putStrLn "Hello World!"
   putStrLn "Hello, what's your name?"
   name <- getLine
   putStrLn ("Hey " ++ name ++ ", you rock!")

-- Other useful functions include
--    putStr
--    putChar
--    print
--
--    getChar
--    when - from Control.Monad
--    sequence - takes a list of I/O actions and returns an
--       I/O action that will perform those actions one after another
--
-- FILE I/O !!!
--    openFile
--    getContents
