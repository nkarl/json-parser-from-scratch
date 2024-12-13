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
  = Wrapper WRAP
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

{--
  NOTE: need a custom monad, to multiplex different lexing functions.

  For example,
    - when seeing the `"` symbol, multiplex to lexString.
    - When seeing the `[` symbol, multiplex to lexArray.
    - when seeing the `{` symbol, multiplex to lexObject.

  The last 2 are complex, because we compose them from lexString recursively.

  Therefore, the monad must support a polymorphic type that can track 3 separate states:
    1. the state while lexString
    2. the state while lexArray
    3. the state while lexObject

  *assuming that we are composing (lexObject . lexArray . lexString)

  What is my source input? I need a characteristic source input?
--}

-- | the source input, which should be a string of finite length.
type SourceInput = [Char]

-- | the list of tokens to be lexed, which should be finite up to the string's length.
type Tokens = [Token]

-- | the state to be passed between functions inside a Lexer monad.
type State = (SourceInput, Tokens)

-- | the lexer monad to be unified all lexing functions. Results in either an error or a state.
type LexerM = Either ErrorMsg State

type ErrorMsg = String

{--
  With these data types, we have a scheme of handling lexing combinations.
--}

{- | parses a source string continuously until seeing an expected WRAP.

For example:

```
lexString `abc\"` DBLQUOTE mempty == ("abc", "abc")
```

  TODO: modify lexString to accommodate LexerM.
-}
lexString :: State -> Maybe WRAP -> [Char] -> LexerM
lexString state Nothing string = Right state
lexString (x : xs, ts) (Just expect) string
  | matchSymbol x == expect =
      let found = Text (reverse string)
       in lexString
            (xs, MoreWork found : Wrapper expect : ts)
            Nothing
            string
  | otherwise = lexString (xs, ts) (Just expect) (x : string)

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
