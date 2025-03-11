{
module Alexdemo  where
}

%wrapper "basic"

-- Add your token regular expressions and associated actions below.

tokens :-
  $white+                        ;
  \+                             { \s -> Plus }
  \*                             { \s -> Times}
  [0-9]+                         { \s -> Number (read s:: Int)}
  [a-z]+                         { \s -> Identifier s}


{
-- The token type:

data Token = Plus
           | Times
           | Number Int
           | Identifier String
    deriving (Show, Eq)

}