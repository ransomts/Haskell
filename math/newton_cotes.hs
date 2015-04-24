 
--main :: IO ()
--main = do
--  print (nc_integration fa (-1) 4  5)
--  print (nc_integration fb   0 (sqrt(5)) 5)
--  print (nc_integration fc (-1) 1  5)
--  print (nc_integration fd   0  10 5)
--  print (rk4 (fun) 0 10 0.1 10)

fa :: Double -> Double
fa x = exp(cos(x))

fb :: Double -> Double
fb x = 8 * sqrt (1.0-x*x) - x

fc :: Double -> Double
fc x = 1.0 / (1.0+10.0*x*x)

fd :: Double -> Double
fd x = sin(x)/x -- from 0.0 to 10.0

nc_integration :: (Double -> Double) -> Double -> Double -> Double -> Double
nc_integration f a b n =
  ((b - a) / 24) * (11 * (f (a + s)) + f (a + 2*s) + f (a + 3*s) + 11 * (f (a + 4*s)))
  where
    s = (b - a) / n

--k1 :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
k1 f h xn yn = h * (f xn yn)

--k2 :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
k2 f h xn yn = h * (f (xn + h/2) (yn + (k1 f h xn yn) / 2))

--k3 :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
k3 f h xn yn = h * (f (xn + h/2) (yn + (k2 f h xn yn) / 2))

--k4 :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
k4 f h xn yn =  h * (f (xn + h) (yn +  (k3 f h xn yn)))
{-
rk4 f x y h n
 | n == 0 = y
 | n >  0 = (rk4 f x y h (n-1)) + (k1 f h x y)/6 + (k2 f h x y)/3 + (k3 f h x y)/3 + (k4 f h x y)/6
-}
fun t y = t * sqrt(y)

 
dv :: Floating a => a -> a -> a
dv = (. sqrt). (*)
 
fy t = 1/16 * (4+t^2)^2
 
rk4 :: (Enum a, Fractional a)=> (a -> a -> a) -> a -> a -> a -> [(a,a)]
rk4 fd y0 a h = zip ts $ scanl (flip fc) y0 ts  where
  ts = [a,h ..]
  fc t y = sum. (y:). zipWith (*) [1/6,1/3,1/3,1/6]
    $ scanl (\k f -> h * fd (t+f*h) (y+f*k)) (h * fd t y) [1/2,1/2,1]
 
task =  mapM_ print
  $ map (\(x,y)-> (truncate x,y,fy x - y)) 
  $ filter (\(x,_) -> 0 == mod (truncate $ 10*x) 10) 
  $ take 101 $ rk4 dv 1.0 0 0.1

-- 101 steps taken
-- y0 = 1
-- t0 = 0
-- dt = 0.1
