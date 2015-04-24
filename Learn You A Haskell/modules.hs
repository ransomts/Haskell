
-- Modules
--    Collections of related functions, types, and typeclasses
--    Haskell standard library is split into modules, each serving some
--    common purpose
--
--    In the ghci and ghc, the Prelude module is loded on defualt
--
--    modules are loaded with the import keyword
--       import <module name>
--    usually at the top, but must be before defining any functions
--
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- Here, the nub function removes all duplicates in the list and is 
--    composed in the length function

-- To load in modules into ghci, just use the :m command,
   --      for example ghci> :m + Data.List Data.Map Data.Set
   --    you can also selectively load in only the funcitons 
   --    you want
--      for example ghci> :m + Date.List (nub, sort)
   --    or do the complement of that import with the hiding keyword
--      ghci> :m + Date.List (nub)
   --
   --    To be able to reference a module without using all the funciton
   --    names, we can used the qualified keyword
   --
   -- Modules are there so the code is short, succinct, easy to read, and all 
   --    the great reasons librarys exist in code
   --
   --
   --    Good modules to know, Data.List, Data.Char, Data.Map, Data.Set
   --

   -- Custom modules can of course be made, usually in a different file, but
   --    with this syntax
   module Geometry (
         sphereVolume ,
         sphereSurfaceArea ,
         cubeVolume ,
         cubeSurfaceArea
         ) where

   sphereVolume :: Float -> Float
sphereVolume x = pi * ( x^3 ) * (4.0/3.0)

   -- And the other function definitions
   --
-- To import this into another file, put the title of the module ( without the .hs ) 
   --     next to an import so long as they are in the same folder
