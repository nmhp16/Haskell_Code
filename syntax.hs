import Data.Array (Ix(range))

-- Int -- bounded, word-size integer
-- Integer -- unbounded integer
-- Float -- floating point with single precision
-- Double -- floating point with double precision
-- Bool -- boolean
-- Char -- character
-- () -- unit type (void or NoneType)
-- [a] -- list (of type a)
-- String -- string -- same as [Char]
-- (a, b) -- tuple of types a and b


-- The Basics: Functions
square :: Double -> Double -- type signature
square x = x * x -- :t square => square :: Double -> Double

-- The Basics: Anonymous Functions
square' :: Double -> Double -- type signature
square' x = x * x -- :t square' => square' :: Double -> Double

-- The Basics: Lists
-- Lists in Haskell are immutable and homogenous
-- xs = [1, 2.4, "Hello"] => Error
-- xs = [1, 2.4, 4] => [1.0, 2.4, 4.0]
-- :t xs => xs :: Fractional a => [a], Fractional is a typeclass that includes Float and Double types.

xs = [1, 2.4, 4]
-- head xs => 1.0
-- tail xs => [2.4, 4.0]

-- No cons operator in Haskell to add an element to a list
-- Instead, use the : operator
-- xs = 1 : 2 : 3 : [] => [1, 2, 3]
concatenatedList = [1, 2, 3] ++ [1, 2, 3] -- [1, 2, 3, 1, 2, 3]
rangeNum = [1..10] -- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
rangeChar = ['a'..'z'] -- "abcdefghijklmnopqrstuvwxyz"
rangeStep = [1, 3..20] -- [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
x:y:z = xs -- x = 1, y = 2.4, z = 4.0
i:_:j = xs -- i = 1, j = 4

-- Pattern Matching
-- Pattern matching is a way to destructure data
-- It is a way to bind variables to parts of a data structure
factorial :: Integer -> Integer -- type signature
factorial 0 = 1 -- base case
factorial n = n * factorial (n - 1) -- recursive case
-- if do factorial 0 = 1 after factorial n = n * factorial (n - 1) then it will throw an error because n is not defined when n = 0 

-- Pattern Matching with Lists
swap :: [a] -> [a] -- type signature
swap [] = [] -- base case
swap [x] = [x] -- base case
swap (x:y:rest) = y:x:rest -- recursive case

main :: IO () -- type signature
main = do -- IO action
    print (square 3) -- 9.0
    print (square 4) -- 16.0

-- To run in terminal:
-- stack ghci
-- :load syntax.hs