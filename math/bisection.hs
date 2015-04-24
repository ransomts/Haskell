
{- bisection :: Floating a => (a -> a) -> a -> a -> a -> a
bisection f a b e = if a < b then bis a b else bis b a
    where err     = if e < 1e-15 then 1e-15 else e
          bis a b = let d = (b - a) / 2; m = (b + a) / 2 in
                    if d < err then
                        m
                    else if f a * f m < 0.0 then
                        bis a m
                    else
                        bis m b
-}

-- This is just the type declaration, letting Haskell know were working with
--             A function            and a few Doubles             and returning a Double
regulaFalsi :: (Double -> Double) -> Double -> Double -> Double -> Double
regulaFalsi f a b err 
      -- Base case, where the function value at c is less than the error
      | abs (f c) < err = c
      -- Move b closer
      | f a * f c  < 0 = regulaFalsi f a c err
      -- Move a closer
      | f a * f c >= 0 = regulaFalsi f c b err
   where c = (a * (f b) - b * (f a)) / ((f b) - (f a))


bisection :: (Double -> Double) -> Double -> Double -> Double -> Maybe Double
bisection f a b err
  | err < 1e-15           = Nothing
  | abs (b - a) / 2 < err = Nothing
  | a < b                 = bis a b
  | otherwise             = bis b a
  where
    bis a b
      | d < err || f m == 0.0 = Just m
      | f a * f m < 0.0       = bis a m
      | f m * f b < 0.0       = bis m b
      | otherwise             = Nothing
      where
        d = (b - a) / 2
        m = (b + a) / 2

main :: IO ()
main = do
  print (regulaFalsi (\x -> x) (-1) 1 1)
  print (bisection (\x -> x) (-1) 1 1)
