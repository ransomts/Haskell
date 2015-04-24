
--f :: Int -> Int -> Int
f :: Num a => a -> a -> a
f x y = x * x + y * y

main :: IO ()
main = print (f 2.0 3)
