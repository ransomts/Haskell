
-- Haskell has a static type system, meaning the types of every expression is known at compile
-- time, leading to safer code. Everything in haskell has a type
-- You can check the types of things with :t <whatever> in ghci
--
-- Explicit types always start with a capital letter, eg :t 'a' productes 'a' :: Char
--
-- when writing any sort of actual function, it's good to explicitly declare the type.
--
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- So Int is the ordinary integer type, with 32 bit two's compliment precision
-- you can also use Integer, which is unbounded, but significantly slower
addThree :: Int -> Int -> Int -> Int
addThree x y z -> x + y + z

-- Float is the single precision floating point, Double is double precision
-- then theres Bool, Char, and the empty tuple ()
--
-- Functions can also have type variables for their types, for example
-- the head function :t head produces head :: [a] -> a 
-- because a is lowercase, it is not an ordinary type, and the head 
-- function is considered to be polymorphic

-- TYPE CLASSES
-- typeclasses define behaviors.
-- :t (==) produces (==) :: (Eq a) => a -> a -> Bool
--
-- this brings in the => symbol:
--    everything before the => is a class constraint, and the type of ==
--    can be read as " the equality function takes any two values that are 
--    of the same type and returns a Bool. The type of those two values must 
--    be a member of the Eq class.
-- The Eq typeclass is an interface for testing equaltiy. All standard Haskell
--    types except for IO and function are part of the Eq typeclass
--
-- Basic typeclasses:
--    Eq - types that support tequality testing
--    Ord - types that have an ordering
--    Show - types that can be presented as strings
--    Read - types that can be read in as a string
--       provides a counter to the show typeclass
--       we can use read in order to intergret some data
--       read "5" :: Int
--       read "5" :: Float 
--       read "5" :: [Char]
--       we need to specify the type that we want the input to be
--       interpreted as in order to clear up some type smoke
--    Enum - types that can be enumerated, for example ['a'..'z']
--       this typeclass is the one that we can call succ and pred on
--    Bounded - types that have a lower and upper bound, like Int, Char, Bool
--       these bounds can be checked with the functions minBound and maxBound
--       tuples are all Bounded
--    Num - types that are numeric, Int, Integer, Float, Double, etc.
--    Integral - types that are in the subset of integral (whole) numbers
--    Floating - types in the complement of the Integral typeclass, so floating point numebers
--       Converting from a Num to an Integral can cause some problems that are fixed 
--       by using the fromIntegral :: (Num b, Integral a) => a -> b
