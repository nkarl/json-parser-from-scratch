module Lexer where

{-
  TODO: [x] modify lexString to accommodate LexerM.
        [x] map out the edge cases of lexString.
        [x] flatten the data type `Token` so I have to type less.
        [ ] compose lexString with an enclosure that joins/creates a continuation
            between `matchChar` and the opening `Wrap`. This allows for filtering
            into different lexing functions (Number, String, Array, Object).
            Also, needs to comose them in order to define the enclosure recursively.
        [ ] writes a lexer function (maybe inside runLexer) to lex with a predicate.
            This should allow for composition of predicates, and it should try to use
            the Alternative functor.
-}

{--
  NOTE
  The data tree is separated into 3 subtrees.
    1. `Wrap`: handles all the wrapper symbols that deal with
      - objects: '{' and '}'
      - arrays: '[' and ']'
      - strings: '"' (symmetric and reflexive?)

    2. `Delimit`: handles separation of ts or groups of ts.
      - WhiteSpace: is ignored
      - Comma: separate any two elements (a simple push/cons)
      - Colon: separate a pair of key-value

    3. Complex data types that need additional work to transform from a valid String to the following
      - STRING String
      - NUMBER Int
      - BOOLEAN Bool
--}

import Prelude

data MetaToken
  = DBL_QUOTE
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | WHITESPACE
  | COMMA
  | COLON
  | OTHER
  deriving (Show, Eq)

data Token
  = META MetaToken
  | STRING String
  | NUMBER Int
  | BOOLEAN Bool
  deriving (Show, Eq)

matchChar x = case x of
  '"' -> DBL_QUOTE
  '[' -> LEFT_BRACKET
  ']' -> RIGHT_BRACKET
  '{' -> LEFT_BRACE
  '}' -> RIGHT_BRACE
  ' ' -> WHITESPACE
  ',' -> COMMA
  ':' -> COLON
  _ -> OTHER

-- | the source input, which should be a string of finite length.
newtype Source = Source [Char]

-- | the list of tokens to be lexed, which should be finite up to the string's length.
type Tokens = [Token]

-- | the state to be passed between functions inside a Lexer monad.
type State = (Source, Tokens)

-- | the lexer monad to be unified all lexing functions. Results in either an error or a state.
type LexerM = Either ErrorMsg State

data ErrorMsg
  = EOF
  | Unterminated Token
  deriving (Show, Eq)

{- | total enclosure for all lexing functions and states.

  For example,
    1. when seeing the `"` symbol, multiplex to lexString.
    2. When seeing the `[` symbol, multiplex to lexArray.
    3. when seeing the `{` symbol, multiplex to lexObject.

  The last 2 are complex, because we compose them from lexString recursively.

  Therefore, the monad must support a polymorphic type that can track 3 separate states:
    1. the state while lexString
    2. the state while lexArray
    3. the state while lexObject

  *assuming that we are composing (lexObject . lexArray . lexString)
-}
runLexer :: State -> LexerM
runLexer (Source mempty, []) = Left EOF
runLexer state =
  let
    (Source (x : xs), ts) = state
   in
    case matchChar x of
      WHITESPACE -> runLexer (Source xs, ts)
      DBL_QUOTE -> lexString state (Just DBL_QUOTE) mempty
      LEFT_BRACE -> lexArray state (Just RIGHT_BRACE) mempty
      LEFT_BRACKET -> lexObject state (Just RIGHT_BRACKET) mempty

{- | TODO: needs to be composed from lexString and lexNumber

__MetaTokens:__ `LEFT_BRACE`, `RIGHT_BRACE`, `COLON`, `COMMA`
-}
lexObject :: State -> Maybe MetaToken -> [Token] -> LexerM
lexObject (Source [], _) (Just terminal) _ = Left $ Unterminated $ META terminal
lexObject state Nothing _ = Right state
lexObject (Source (x : xs), ts) (Just terminal) obj = undefined -- TODO

{- | TODO needs to be composed from lexString

__MetaTokens:__ `LEFT_BRACKET`, `RIGHT_BRACKET`, `COMMA`
-}
lexArray :: State -> Maybe MetaToken -> [Token] -> LexerM
lexArray (Source [], _) (Just terminal) _ = Left $ Unterminated $ META terminal
lexArray state Nothing _ = Right state
lexArray (Source (x : xs), ts) (Just terminal) arr = undefined -- TODO

{- | TODO: need to tokenize any integers.

__MetaTokens:__ `COMMA`
-}
lexNumber = undefined -- TODO

{- | parses a source string continuously until seeing an expected MetaToken Wrap.

__MetaTokens:__ `DBL_QUOTE`

__Example 0:__

@
input = ""
reult = lexString input (Just DBL_QUOTE) mempty
result == Left $ Unterminated (DBL_QUOTE) -- >>> True
@
__Example 1:__

@
input = "abc\\""
reult = lexString input (Just DBL_QUOTE) mempty
result == ("", STRING "abc") -- >>> True
@
-}
lexString :: State -> Maybe MetaToken -> [Char] -> LexerM
lexString (Source [], _) (Just terminal) _ = Left $ Unterminated $ META terminal
lexString state Nothing _ = Right state
lexString (Source (x : xs), ts) (Just terminal) str
  | matchChar x == terminal =
      lexString (Source xs, makeStrToken str <> ts) Nothing str
  | otherwise =
      lexString (Source xs, ts) (Just terminal) (x : str)
 where
  makeStrToken :: String -> [Token]
  makeStrToken [] = [terminated]
  makeStrToken xs = [STRING (reverse xs), terminated]
  terminated = META terminal
