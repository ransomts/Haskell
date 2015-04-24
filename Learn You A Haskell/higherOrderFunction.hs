
-- Any function that takes another function as a parameter is considered a 
--    higher order function. That is mostly everything that Haskell is based on.
-- 

-- Curried functions - every function takes more than one parameter is a 
--    curried function. This means that these two are actually equivalent
--    max 4  5
--   (max 4) 5
--    because putting a space between two things in Haskell is a function 
--    application. When we look at the type of the max function, it spits 
--    out max :: (Ord a) => a ->  a -> a which can also be written as
--        max :: (Ord a) => a -> (a -> a) which could be read as max takes
--    something in the Ord typeclass called a and return a function that 
--    takes another a and returns an a.

--    So when a curried function is called with too few parameters, it actually
--    does what it can with what was given, which is a partially applied function.
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--------------------------------------------------------------------------------
-------------------------- THIS IS REALLY IMPORTANT ----------------------------
--------------------------------------------------------------------------------

-- Maps take a function and a list and applies that function to the list
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- Filters are funcitons that take a predicate and a list and returns the the subset
--    of the list that satisfies the predicate
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
   | p x = x : filter p xs
   | otherwise = filter p xs
-- Common filter would be the takeWhile, which takes from the head and continues
--    until the predicate fails, and stops there

-- Lambdas 
-- Anonymous functions that are used because that function is needed only once
--    notated with the '\' symbol because it looks a bit like the greek lambda
--    then the parameters, space deliminated, then a '->' and the function body
--    usually surrounded by parens, just for style

evalFunction :: (RealFloat a) => (a -> a) -> a -> a
evalFunction f x = f x
-- this is just a call I wrote 
-- evalFunction (\x -> (x^2) - 10) 10

-- Folding
-- Takes in a list and a binary operator, applies that opperator recursively
--    across the list then spits out the rest, starting with the value supplied
-- 
-- This function takes a numerical list and returns the sum with a folding aprroach
-- sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- Function composition
-- This is the mathematical function composition f( g(x) ) 
-- Composition is done with the ( . ) operator eg (f . g) x, has 
-- all the same usefull properties as compositon in math
