-- Version 1
evenSum :: [Integer] -> Integer

evenSum l = accumSum 0 l
accumSum n l = if l == []
                  then n
                  else let x = head l
                           xs = tail l
                       in if even x
                              then accumSum (n+x) xs
                              else accumSum n xs

-- Version 2
evenSum2 :: Integral a => [a] -> a
evenSum2 l = accumSum 0 l
     where  accumSum n l = if l == []
                           then n
                           else let x = head l
                                    xs = tail l
                                in if even x
                                   then accumSum (n+x) xs
                                   else accumSum n xs

-- Version 3
evenSum3 l = accumSum 0 l
    where
        accumSum n [] = n
        accumSum n (x:xs) =
             if even x
                then accumSum (n+x) xs
                else accumSum n xs

-- Version 4
evenSum4 :: Integral a => [a] -> a
evenSum4 = accumSum 0
    where
        accumSum n [] = n
        accumSum n (x:xs) =
             if even x
                then accumSum (n+x) xs
                else accumSum n xs

-- Version 5
evenSum5 l = mysum 0 (filter even l)
    where
      mysum n [] = n
      mysum n (x:xs) = mysum (n+x) xs

myEvenSum :: Integral a => [a] -> a
myEvenSum l = sum $ filter (even) l

main :: IO ()
main = print $ evenSum [1..100]
