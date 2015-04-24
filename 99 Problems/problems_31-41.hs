
-- Determine if a givin integer is prime
primesToG :: (Integral a) => a -> [a]
primesToG m = 2 : sieve [3,5..m]  where
    sieve (p:xs) 
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])
                  -- p : sieve (xs `minus` map (p*) [p,p+2..])
                  -- p : eulers (xs `minus` map (p*) (p:xs))
