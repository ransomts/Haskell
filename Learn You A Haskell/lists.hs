
lostNumbers :: [Int]
lostNumbers = [4, 8, 15, 16, 23, 42]

-- Two lists can be concatenated with ++
-- eg [1..10] ++ [20..30]
-- or ['w', 'o'] ++ ['o', 't']

-- Pull out specific indexed elements with !!
-- eg [0, 1, 2, 3, 4, 5] !! 1 = 1

-- comparatives between lists are compared 
-- lexicographically
-- [3, 2, 1] < [2, 1, 0] = false

-- Head     Tail
--  |  {-----|-----}
-- [5, 4, 3, 2, 1, 0]
--  {-----|-----}  |
--      init      last
-- these can be called with
-- head [list]

-- null [list] checks if a list is null
-- eg null []  = true
--    null [1] = false
--

-- reverse [1, 2, 3] = [3, 2, 1]
-- take 3 [1, 2, 3, 4, 5] = [1, 2, 3]
-- drop 3 [1, 2, 3, 4, 5] = [4, 5]
-- maximum [1, 2, 3, 4, 5] = 5
-- minimum [1, 2, 3, 4, 5] = 1
-- sum [1, 2, 3, 4, 5] = 15
-- minimum [1, 2, 3, 4, 5] = 120
-- 4 `elem` [1, 2, 3, 4, 5] = True


-- RANGES
 [1..20]
 [2, 4..20]
 ['a'..'z']
--
-- cycle make an infinite list of what it's given
 take 10 (cycle [1, 2, 3])
--    [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
-- take 5 (repeat 5) -- cycle for one element
-- this take example is the same as 
-- replicate 5 5

-- LIST COMPREHENSION
--
-- basically set builder notation
--
 [x*2 | x <- [1..10]]
 [x*2 | x <- [1..10], x*2 >= 12]
 [x   | x <- [50..100], x `mod` 7 == 3]
boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]
 boomBangs [7..13]
 [x | x <- [10..20], x /= 11, x /= 15]

 -- rewriting the length function
length' :: String -> Int
length' xs = sum [1 | _ <- xs]
 -- toUpper
toUpper :: String -> String
toUpper st = [ c | c <- st, c `elem` ['A'..'Z']]

 -- TUPLES
 -- Tuples and n-ples group together relevant bits of information
 -- eg ("Tim", "Ransom", 19)
 fst ("Tim", "Ransom") -- this and snd only work on 2-ples not on n-ples
 -- zip will group two lists into one list of tuples
 -- zip [1, 2, 3, 4, 5] [5, 5, 5, 5, 5]
 -- [(1, 5), (2, 5), (3, 5), (4, 5), (5, 5)]
 --
 -- zip can be used with infinite lists too
 -- zip [1..] ["I'm", "a", "turtle"]
 
 -- let rightTriangles' = [ (a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10], 
 --   a^2 + b^2 = c^2 ]
 
