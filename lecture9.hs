{- Lecture 9
Basic Examples -}

-- Bindings
first = 1
second = 4
third = first + second -- bind third to the sum

-- -- This will throw an error because first is already bound
-- first = 0
-- third = first + second -- bind third to the sum

main :: IO ()
main = do
    putStrLn "Values:"
    print first
    print second
    print third