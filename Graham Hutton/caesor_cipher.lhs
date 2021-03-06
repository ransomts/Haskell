
Encoding and Decoding

So we need a pair of functions that will take us from characters to ints and back. 
For simplicity, only lower case letters will be used

\begin{code}

import Data.Char hiding (toLower)
--import Data.Text
import Data.String as String
import Data.List as List


char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

\end{code}

And using these two functions, we can make a shift function

\begin{code}

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2char ((char2int c + n) `mod` 26)
  | otherwise = c

\end{code}

Using shift within a string comprehension, it is now easy to 
define a function that encodes a string using a given shift factor

\begin{code}

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = [shift (26-n) x | x <- xs]

\end{code}

Frequency Tables

The key to cracking the Caesar cipher is the observation that some 
letters are used more frequently than others in English. By analyzing 
a large volume of text, one can derive the following table of 
approximate percentage frequencies of the twenty-six letters of the alphabet

This is the frequency table of the English language, with index 0 
corresponding to 'a', 1 to 'b' etc.

\begin{code}

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1,  6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
     where n = Data.Text.toLower xs

\end{code}

So in order to actually make the comparison from the expected frequencies to the actual numbers, we use the chi-square statistic. The actual math of the statistic will be left for the stat books, all we need to know is that a smaller value is better here. 

\begin{code}

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- List.zip xs [0..n] x == x']
                 where n = length xs -1

crack :: String -> String
crack xs = encode (-factor) xs
    where factor = List.head (positions (minimum chitab) chitab)
          chitab = [chisqr (rotate n table') table | n <- [0..25]]
          table' = freqs xs

\end{code}
