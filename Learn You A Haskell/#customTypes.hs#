
-- data Bool = False | True
-- This is the type definition of the boolean datatype in the standard library

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r^2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2-x1) * (abs $ y2-y1)

-- ways to call this from ghci
-- ghci> surface $ Circle 10 20 10
-- ghci> surface $ Rectangle 0 0 100 100
--

-- This uses a point data type to help clear out the smoke of the type
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangel Point Point deriving (Show)

shift :: Shape -> Float -> Float -> Shape
shift (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
shift (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
   Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))
-- Using the data constructors in the other constructors helps keep the code
--    readable and bug free

-- RECORD SYNTAX
-- So if this is the way we use to store data, how can we easily keep track of 
--    which data field is for which data entry?
data Person = Person {
   firstName :: String ,
   age :: Int ,
   height :: Float ,
   phoneNumber :: String ,
   iceCreamFlavor :: String
} deriving (Show)
-- This notation will also generate functions that will retrieve the data
--    from the type for you for example
-- ghci> :t firstName would return firstName :: Person -> String
--    using this record syntax will also change how the data in the type is 
--    displayed, instead of just spitting out the data in the type, it will 
--    actually have something like <fieldName> = <fieldData>

-- TYPE PARAMETERS
-- The maybe type encapsulates an optional value. A value of type Maybe a either
--    contains a value of type a (represented with Just a) or is empty 
--    (represented with Nothing)
data Car = Car {
   company :: Maybe String ,
   model   :: Maybe String ,
   year    :: Maybe Int
} deriving (Show)

-- Some more on typeclasses
--
-- When a data type is declared, for instance
data Person = Person {
   fname :: String ,
   lname :: String ,
   age   :: Int
} deriving (Eq, Show, Read)
-- It can derive the Eq typeclass and be compared to other instances of the type with 
--    the ordinary == or /= comparisons
--    They can also be used now in functions that require the Eq typeclass, such as elem

-- Show and Read typeclasses
--
-- Show and Read are typeclasses that take things to and from Strings, so if we take the
--    person datatype from above and add the Show and Read typeclasses, we can print out
--    the data to the terminal
-- 
-- ghci> let me = Person { fname = "Tim", lname = "Ransom", age = 19 }
-- ghci> me
--    me = Person { fname = "Tim", lname = "Ransom", age = 19 }
-- ghci> 
--
-- The Read typeclass lets you turn input strings into data
--
-- ghci> read "Person { fname = \"Tim\", lname = \"Ransom\", age = 19 } == me
--    True

-- Ord typeclass
--    Specifies that a type has an order, and so applies to the < > operators
-- ghci> Nothing < Just 100
--    True
-- ghci> Just 3 `compare` Just 2
--    GT

-- Enum and Bounded
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Bounded puts a minimum and a maximum value in the datatype for example
-- ghci> minBound :: Day
--    Monday
-- ghci> maxbound :: Day
--    Sunday
--
-- Enum lets us use the datatype in list ranges as well as giving succ and pred values
-- ghci> succ Monday
--    Tuesday
-- ghci> [minBound .. maxBound] :: [Day]


-- TYPE SYNONYMS
--
-- Types can be isomorphic, for instance String = [Char]
--    Thats great and all, but one of the ways to use it would be to
--    have a phonebook datatype
phoneBook :: [(String, String)]
phoneBook =
   [
    ("mom", "213-0955"),
    ("dad", "213-1605"),
    ("katie", "260-171[")
   ]
-- We can use type synonyms to convey some information in the declaration
--    instead of just saying it maps two Strings together
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)] 
phoneBook :: PhoneBook 
phoneBook =
   [
    ("mom", "213-0955"),
    ("dad", "213-1605"),
    ("katie", "260-171[")
   ]
-- So now when we write functions, we can also use these synonyms
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name, pnum) `elem` pbook
-- When you use monads and don't want to have to type the full name every time
--    you can use
type Intmap = Map.Map Int

-- RECURSIVE DATA STRUCTURES
--
-- Rucursive data types are data that is defined using itself, this may be an implicit
--    datatype depending on further reading
--
--    Implicit datatypes have a least fixedpoint and a closed operator
--
-- So an example of a recursize datatype would be the list, because every list would be
--    either the empty list or the empty list with another list attached to its head
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- Another example would be a tree
data Tree t = Leaf t | Tree (Tree t) (Tree t) deriving (Show, Read, Eq, Ord)

-- An implementation of a binary search tree, or BST
singleton :: a -> Tree a 
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) 
   | x == a = Node x left right
   | x < a  = Node a (treeInsert x left) right
   | x > a  = Node a left (treeInsert right)

-- Typeclasses 102
-- Recap : Typeclasses are there to organize all the types and provide a sort 
--    of interface among common aspects, for instance the Eq typeclass
class Eq a where
   (==) :: a -> a -> Bool
   (/=) :: a -> a -> Bool
   x == y = not (x /= y)
   x /= y = not (x == y)

-- You can define your own instances of the vairious typeclasses when the
--    standard definitions fail, for example the standard Eq fails on a TrafficLight
data TrafficLight = Red | Yellow | Red
instance Eq TrafficLight where
   Red == Red = True
   Yellow == Yellow = True
   Green == Green = True
   _ == _ = False 

instance Show TrafficLight where
   show Red = "Red Light"
   show Yellow = "Yellow Light"
   show Green = "Green Light"

-- Yes - No typeclass, one that sort of emulates the weakly typed system
--    of having an empty string equal false and anything else be true
class YesNo a where 
   yesno :: a -> Bool
-- Notice above how a is defined to be a concrete type, none of that Maybe shit
-- Here is the instance where a is an Int type
instance YesNo Int where
   yesno 0 = False
   yesno _ = True

instance YesNo [a] where
   yesno [] = False
   yesno _ = True

instance YesNo Bool where
   yesno = id
-- id here is a standard library function of the identity function

instance YesNo (Maybe a) where
   yesno (Just _) = True
   yesno Nothing = False
-- All this is showing is that we can define our own typeclass and give instances
--    of the various types and can even handle the Maybe function to give
--    the Nothing type a False value, which here makes sense

-- ==============================================================================
-- The Functor typeclass
-- Ord is for ordered types, Eq is for equatable types
-- Functor is for types that can be mapped over
-- ==============================================================================

-- This is the functor definiton
class Functor f where
   fmap :: (a -> b) -> f a -> f b

-- So functors aren't that confusing actually, an example of one would be the map funciton
--    functors by definition are types of mappings between categories
