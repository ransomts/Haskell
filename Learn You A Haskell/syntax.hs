--
-- Pattern Matching
--
-- When lucky is called, the patterns are matched and checked 
--    from top to bottom. This function only matches to the
--    pattern of 7 or it falls through to the not so lucky part
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky seven!"
lucky x = "Not so lucky!"

-- Notice that the catch all (sayMe x) is at the bottom, if it
--    were on the top, it would execute everytime
sayMe :: (Integral a) => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe 3 = "three"
sayMe 4 = "four"
sayMe 5 = "five"
sayMe x = "Not between 1 and 5"

-- Little bit of recursion here, which is the super important in
--    haskell, this is also another example of pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Here is one way to implement adding two tuple vectors, but we want
--    to do it with pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
--    so here we break out the big pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- fst and snd take out the first and second elements from a tuple, but 
--    here is a way to define them in an extendable way again with 
--    pattern matching
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
--    having the underscore is a way to separate the parts of the list 
--    without having to declare a name for something we don't want to use

head' :: [a] -> a
head' [] = error "Can't take the head off an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "An empty list"
tell (x:[]) = "One element: " ++ show x
tell (x:y:[]) = "Two elements: " ++ show x ++ " " ++ show y
tell (x:y:zs) = " More than two elements " ++ show x ++ " " ++ show y ++ " " tell zs

length' :: (Num b ) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num b) [a] -> b
sum' [] = 0
sum'(x:xs) = x + sum' xs

-- Patterns are a cool thing too, they let you map a name to something while still
--   keeping it intact
--   eg list@(x : y : xs)
--   so now list = (x : y : xs)

-- Gaurds
-- Basically the piecewise notation of Haskell
-- where keyword lets you declare variable names
bmiTell :: (RealFloat a) => a -> String
bmiTell height weight
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "Fat"
  | otherwise   = "Basically a whale"
  where bmi = weight / height ^ 2

-- Backticks `these` are used to move function names from
--    prefix to infix, helping with readability
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
   | a > b = GT
   | a < b = LT
   | otherwise = EQ

-- Where bindings bind variables at the end of a function and 
--    and have a scope of the whole function
initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l] ++ ". "
   where (f:_)  = first
         (l:_)  = last

-- Let bindings bind anywhere and are expressions, so they
--    don't hold across gaurds, they have the form
--    let <bindings>
--    in  <expressions>
cylinderVol :: (RealFloat a) => a -> a -> a
cylinderVol r h =
   let side = 2 * pi * r * h
       top  = pi * r^2
   in side + 2 * top
-- Let's are crazy powerful and can be crammed just about 
--    anywhere you want them, because they are expressions
--    they let you introduce functions as well
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h^2]

-- Case Expressions
--    The switch/case, but allows for pattern matching and evaluation
--    because it is an expression
head' :: [a] -> a
head' xs = case xs of [] -> error "Empty List"
                      (x:_) -> x
--  case <expression> of <pattern> -> <result>
--                       <pattern> -> <result>
--                       <pattern> -> <result>
--                       ...
--  again because this is an expression, it can be put in strange places,
--    and by strange, I mean everywhere
