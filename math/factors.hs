

factors :: Integer -> [Integer]
factors n = lows ++ (reverse $ map (div n) lows)
    where lows = filter ((== 0) . mod n) [1..truncate . sqrt $ fromIntegral n]

main :: IO ()
main = print (factors 200)
