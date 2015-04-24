
{-
Nested comments are defined as such
this is the second line
-}
prod :: [Integer] -> Integer
prod [] = 0
prod (x:xs) = x * product xs


qsort [] = []
qsort (x : xs) = qsort larger ++ [x] ++ qsort smaller
                 where 
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b >  x]


halve [] = ([], [])
halve [a] = ([a], [])
