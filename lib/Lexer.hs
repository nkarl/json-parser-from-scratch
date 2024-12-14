module Lexer where

import Prelude

data Wrap
  = BRACE
  | BRACKET
  | DBLQUOTE
  | Other
  deriving (Show, Eq)

data Delim
  = WHITESPACE
  | COMMA
  | COLON
  deriving (Show, Eq)

data MultiPoint
  = STRING String
  | NUMBER Int
  | BOOLEAN Bool
  deriving (Show, Eq)

data Token
  = Wrapper Wrap
  | Delimiter Delim
  | MoreWork MultiPoint
  | EOF
  deriving (Show, Eq)

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

{- | parses a source string continuously until seeing an expected Wrap.

For example:

```
lexString `abc\"` DBLQUOTE mempty == ("abc", "abc")
```

  TODO: [x] modify lexString to accommodate LexerM.
        [ ] map out the edge cases.
        [ ] flatten the data type `Token` so I have to type less.
        [ ] compose lexString with an enclosure that joins/creates a continuation
            between `matchChar` and the opening `Wrap`. This allows for filtering
            into different lexing functions (String, Array, Object).
            Also, needs to comose them in order to define the enclosure recursively.
-}
lexString :: State -> Maybe Wrap -> [Char] -> LexerM
lexString ([], _) (Just expect) _ = Left "Error: Unterminated String."
lexString state Nothing _ = Right state
lexString (x : xs, tokens) (Just expect) string
  | matchSymbol x == expect =
      let element = MoreWork $ STRING (reverse string)
          terminated = Wrapper expect
       in lexString (xs, element : terminated : tokens) Nothing string
  | otherwise = lexString (xs, tokens) (Just expect) (x : string)

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
