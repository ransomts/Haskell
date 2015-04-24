-- Reverse Polish calculator
import Data.List

solveRPN :: String -> Float
solveRPN expression = head . foldl foldingFunction [] . words expression
   where foldingFunction (x : y : xs) "*" = (x * y) : xs
         foldingFunction (x : y : xs) "+" = (x + y) : xs
         foldingFunction (x : y : xs) "-" = (x - y) : xs
         foldingFunction (x : y : xs) "/" = (x / y) : xs
         foldingFunction (x : y : xs) "^" = (x ** y) : xs
         foldingFunction (x : xs) "ln" = log x : xs
         foldingFunction (xs) "sum" = [sum xs]
         foldingFunction xs numberString = read numberString : xs

-- Heathrow to London
-- Solving a shortest path problem can be broken into a few steps
--
-- Think about solving the problem by hand
-- Think about how to represent the data in Haskell
-- Think about how to operate on the data
