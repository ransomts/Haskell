data NestedList = Elem a | List [NestedList a]
-- Pulls out the tail from a list
-- 1
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (_:x) = myLast x

-- Pulls out the second to last element 
-- 2
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [x,y] = x
myButLast (_:x) = myButLast x

-- Find the K'th element of a list, 1 indexed
-- 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt (_:xs) i = elementAt xs (i - 1)
elementAt _ _      = error "Index out of bounds"

-- Find the number of element in a list
-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = (myLength xs) + 1

-- Reverse a list
-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h : t) = (myReverse t) ++ [h]

-- find out whether a list is a palindrome
-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == (myReverse xs)

-- Flatten a nested list structure
-- 7
-- Need to define a data type because lists in Haskell
--    are all homogeneous, this lets us have a list with
--    different things in it
myflatten :: NestedList a -> [a]
myflatten (Elem a) = [a]
myflatten (List (x:xs)) = myflatten x ++ myflatten (List xs)
myflatten (List []) = []

