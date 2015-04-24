{-
-- Type Construction
type Name = String
type Color = String

showInfos :: Name -> Color -> String
showInfos name color = "Name: " ++ name ++ ", Color: " ++ color

nameInstance :: Name
nameInstance = "Robin"
colorInstance :: Color
colorInstance = "Blue"

main :: IO ()
main = putStrLn $ showInfos nameInstance colorInstance
-}
-- This doesn't quite provide anything more than aliases

data Name = NameConstr String
data Color = ColorConstr String

showInfos :: Name -> Color -> String
showInfos (NameConstr name) (ColorConstr color) =
  "Name: " ++ name ++ ", Color: " ++ color

nameI :: Name
nameI = NameConstr "Robin"
colorI :: Color
colorI = ColorConstr "Blue"

main :: IO ()
main = putStrLn $ showInfos nameI colorI
