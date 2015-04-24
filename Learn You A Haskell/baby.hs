-- Here is an example of some basic function definitions, with some a string example

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = doubleSmallNumber x + 1

conanO'Brian = "Here Conan is showing that ' is a valid character in a variable name"



