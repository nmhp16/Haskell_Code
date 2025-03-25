skip :: [a] -> [a]
skip [] = []
skip [x] = [x]
skip (x:_:xs) = skip xs

wedge :: a -> [a] -> [a]
wedge _ [] = []
wedge _ [y] = [y]
wedge x (y:ys) = y : x : wedge x ys

mix :: [a] -> [a] -> [a]
mix [] ys = ys
mix xs [] = xs
mix (x:xs) (y:ys) = x : y : mix xs ys

splitBy :: (a -> Bool) -> [a] -> ([a],[a])
splitBy _ [] = ([],[])
splitBy p (x:xs)
    | p x = (x:ys, zs) 
    | otherwise = ([], x:xs)
    where (ys, zs) = splitBy p xs 

data Expression = Atom Double
                | Plus [Expression]
                | Times [Expression]
            deriving Show

evaluate :: Expression -> Double
evaluate (Atom x) = x
evaluate (Plus xs) = foldl (+) 0 (map evaluate xs) 
evaluate (Times xs) = foldl (*) 1 (map evaluate xs)

-- map evaluate xs -- evaluates each expression in the list xs
-- and returns a list of the results