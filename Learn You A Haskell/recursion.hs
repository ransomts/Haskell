
-- Unlike in imperative languages, because Haskell defines what
--    something is rather than how to do it, we use retursion
--    instead of while or for loops

-- Recursion is still the bomb though
maximum' :: (Ord a) => [a] ->
maximum' [] = error "No max of an empty list"
maximum' [x] = x
maximum' (X:rest) =
   | x > maxTail = x
   | otherwise   = maxTail
   where maxTail = maximum' rest

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
   | n <= 0 = []
   | otherwise = x:replicate' (n-1) x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
   let smallerSorted = quicksort [a | a <- xs, a <= x]
       biggerSorted  = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted
