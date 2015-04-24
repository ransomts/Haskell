
import Prelude

main :: IO ()
main = print [(a, b, c, d, e) | a <- [1..25], b <- [a..25], c <- [b..25], d <- [c..25], e <- [d..25], a^5 + b^5 + c^5 + d^5 > 20]

