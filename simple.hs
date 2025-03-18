-- Step 1: Implement Scanner

data Token = Plus
    | Times
    | Number Int
    | Identifier String 
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize s = alexScanTokens s

-- Step 2: Recognizer
recognize :: [Token] -> Bool
recognize ts = let r = rexpr ts
    in case r of
        Just _ -> True
        Nothing -> False

rexpr :: [Token] -> Maybe Token
rexpr [] = Nothing
rexpr (Plus:ts) = rexpr ts
rexpr (Times:ts) = rexpr ts
rexpr (Number n:ts) = rexpr ts
rexpr (Identifier s:ts) = rexpr ts  

-- Step 3: Parser
parse :: [Token] -> Maybe Token
parse ts = let r = rexpr ts
    in case r of
        Just _ -> r
        Nothing -> Nothing

-- Step 4: Interpreter
eval :: Token -> Int
eval (Number n) = n 
eval _ = 0

main = do
    s <- getLine
    let ts = tokenize s
    let r = parse ts
    case r of
        Just t -> print (eval t)
        Nothing -> print "Parse Error"
