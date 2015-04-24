
main :: IO ()
main = print $ f 2 3

default (Int)
--f :: Integer -> Integer -> Integer
f :: Num a => a -> a -> a
f x y = x^2 + y^2

{-
The written type	Its meaning
Int	        the type Int
Int -> Int	the type function from Int to Int
Float -> Int	the type function from Float to Int
a -> Int	the type function from any type to Int
a -> a	        the type function from any type a to the same type a
a -> a -> a	the type function of two arguments of any type a to the same type a
-}

{-
Currying is a thing, meaning that a function can be partially applied
in order to let us think about functions as only ever having one argument
-}
