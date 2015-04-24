
main :: IO ()
main = do
  print "What is you name?"
  name <- getLine
  print "Where do you live?"
  location <- getLine
  print $ location ++ " is a great place to be, " ++ name ++ "!"
  
{-
   print "What is your name?"
   name <- getLine
   print ("Hello " ++ name ++ "!")
-}
-- The main function having the type main :: IO () means that main will cause side effects
