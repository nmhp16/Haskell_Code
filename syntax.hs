import Data.Array (Ix(range))
import Data.String (IsString)

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

-- Pattern Matching (Always idiomatic)
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
swap (first:second:rest) = second:first:rest -- recursive case

-- Guards 
-- Guards allow us to define functions ased oncondition
-- Syntax:
letterGrade :: Int -> String
letterGrade x
    | x >= 90 = "A"
    | x >= 80 = "B"
    | x >= 70 = "C"
    | x >= 60 = "D"
    | otherwise = "F"

-- Bindings with where (Use with Guards)
-- Syntax:
letterGrade' :: Double -> Double -> Double -> String
letterGrade' x y z
    | average >= 90 = "A"
    | average >= 80 = "B"
    | average >= 70 = "C"
    | average >= 60 = "D"
    | otherwise = "F"
    where average = (x + y + z) / 3

-- Let Bindings
-- Syntax:
sumOfSqares x y = let xsquare = x * x
                      ysquare = y * y  
                    in xsquare + ysquare

-- Case Expressions
-- Syntax:
letterGradeCase :: Int -> String
letterGradeCase x = case x of
    90 -> "A"
    80 -> "B"
    70 -> "C"
    60 -> "D"
    _ -> "F"

index :: Eq a => a -> [a] -> Int
index _ [] = -1 -- Index of whatever is not in the empty list is -1
index y (x:xs)
    | y == x = 0
    | otherwise = let indexRest = index y xs
                    in case indexRest of 
                        -1 -> -1 
                        _ -> 1 + indexRest

-- Maybe Type
-- Syntax:
-- Haskell provides Maybe values, which allows us to deote missing results with Nothing. 
-- Similar to Option in Scala
-- Useful when computations fail to generate results

-- Lazy Evaluation
-- Syntax:
-- Haskell is a lazy language, which means that it evaluates expressions only when they are needed.
-- Arguments are not evaluated before they are passed to a function
-- But only when their values are actually used.

-- Lists are also lazy
-- xs = [1..]
-- take 3 xs => [1, 2, 3]
-- length[1..7] => 7

-- Functions are also lazy

-- Higher-Order Functions
-- Syntax:
-- map :: (a -> b) -> [a] -> [b] : map square [1..10] => [1.0, 4.0, 9.0, 16.0, 25.0, 36.0, 49.0, 64.0, 81.0, 100.0]
-- filter :: (a -> Bool) -> [a] -> [a] : filter even [1..10] => [2, 4, 6, 8, 10]
-- foldl :: (a -> b -> a) -> a -> [b] -> a : foldl (+) 0 [1, 2, 3] => 6
-- zip :: [a] -> [b] -> [(a, b)]  : zip [1, 2, 3] [4, 5, 6] => [(1, 4), (2, 5), (3, 6)]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] : zipWith (+) [1, 2, 3] [4, 5, 6] => [5, 7, 9]

-- Main Function
main :: IO () -- type signature
main = do -- IO action
    print (square 3) -- 9.0
    print (square 4) -- 16.0

-- To run in terminal:
-- stack ghci
-- :load syntax.hs
-- :r -- reload