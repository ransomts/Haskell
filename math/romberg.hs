
romberg :: (RealFloat a) => (a -> a) -> (a, a) -> a -> a -> a
romberg f (a, b) 0 0 = 0.5 * (b - a) * ((f a) + (f b))
romberg f (a, b) n 0 = 
   0.5 * (romberg f (a, b) (n-1) 0) + sigma (f 1 2^(n-1)
--romberg f (a, b) n m = (4^m * (romberg f a b n (m-1)) - (romberg f a b (n-1) (m-1)) / (4^m - 1)

-- the sum of f with indicies i through k
sigma :: (Enum a, Num b) => (a -> b) -> a -> a -> b
sigma f i k = sum [ f x | x <- [i .. k] ] 
