-- Some simple program to learn about some haskell stuff
-- Tim Ransom 9.2.15

import System.Environment

-- | 'main' runs the main program
main :: IO ()
main = getArgs >>= print . bar . head

bar s = "bar! " ++ s
