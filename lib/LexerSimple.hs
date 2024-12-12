module LexerSimple where

import Prelude

data WRAP
  = BRACE
  | BRACKET
  | DBLQUOTE
  | Other
  deriving (Show, Eq)

data DELIM
  = WhiteSpace
  | Comma
  | Colon

data MULTIPOINT
  = Text String
  | Number Int
  | Boolean Bool

data Token
  = Wrapper (Maybe WRAP, Maybe WRAP)
  | Delimiter DELIM
  | MoreWork MULTIPOINT
  | EOF

type Source = [Char]
type Output = [Char]

matchSymbol x = case x of
  '"' -> DBLQUOTE
  '[' -> BRACKET
  '{' -> BRACE
  _ -> Other

{- | parses a source string continuously until seeing an expected WRAP.

For example:

```
lexString `abc\"` DBLQUOTE mempty == ("abc", "abc")
```
-}
lexString :: Source -> WRAP -> Output -> (Source, Output)
lexString (x : xs) expect word
  | matchSymbol x == expect = (xs, reverse word)
  | otherwise = lexString xs expect (x : word)

{--
  NOTE
  The data tree is separated into 3 subtrees.
    1. `Wrap`: handles all the wrapper symbols that deal with
      - objects: '{' and '}'
      - arrays: '[' and ']'
      - strings: '"' (symmetric and reflexive?)

    2. `Delimit`: handles separation of tokens or groups of tokens.
      - WhiteSpace: is ignored
      - Comma: separate any two elements (a simple push/cons)
      - Colon: separate a pair of key-value

    3. Complex data types that need additional work to transform from a valid String to the following
      - Text String
      - Number Int
      - Boolean Bool
--}
