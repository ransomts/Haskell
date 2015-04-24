
Encoding and decoding
For simplicity, onle lower case letters will be part of this program

So we need a pair of functions to take us from letters to numbers and back

\begin{code}

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

\end{code}
