
index :: Eq a => a -> [a] -> Maybe Int
index _ [] = Nothing -- Index of whatever is not in the empty list is -1
index y (x:xs)
    | y == x = Just 0
    | otherwise = let indexRest = index y xs
                    in case indexRest of 
                        Nothing -> Nothing 
                        Just i -> Just (1 + i) 

-- Built-in Functions
-- 20 `max` 5 => 20
-- max 20 5 => 20
-- elem 'o' "Hello" => True
-- not elem 'o' "Hello" => False
-- 3 `index` [1, 2, 3] => Just 2
-- 4 `index` [1, 2, 3] => Nothing
-- 20 + 5 => 25
-- (+) 20 5 => 25
