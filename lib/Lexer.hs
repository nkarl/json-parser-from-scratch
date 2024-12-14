module Lexer where

{-
  TODO: [x] modify poopString to accommodate LexerM.
        [ ] map out the edge cases.
        [ ] flatten the data type `Token` so I have to type less.
        [ ] compose poopString with an enclosure that joins/creates a continuation
            between `matchChar` and the opening `Wrap`. This allows for filtering
            into different lexing functions (Number, String, Array, Object).
            Also, needs to comose them in order to define the enclosure recursively.
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
      - Text String
      - Number Int
      - Boolean Bool
--}

{--
  NOTE need a custom monad, to multiplex different lexing functions.

  For example,
    - when seeing the `"` symbol, multiplex to poopString.
    - When seeing the `[` symbol, multiplex to poopArray.
    - when seeing the `{` symbol, multiplex to lexObject.

  The last 2 are complex, because we compose them from poopString recursively.

  Therefore, the monad must support a polymorphic type that can track 3 separate states:
    1. the state while poopString
    2. the state while poopArray
    3. the state while lexObject

  *assuming that we are composing (lexObject . poopArray . poopString)

  What is my source input? I need a characteristic source input?
--}

import Prelude

data MonoChar
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

data PolyChar
  = STRING String
  | NUMBER Int
  | BOOLEAN Bool
  deriving (Show, Eq)

data Token
  = MONO MonoChar
  | POLY PolyChar
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

-- | the list of ts to be lexed, which should be finite up to the string's length.
type Tokens = [Token]

-- | the state to be passed between functions inside a Lexer monad.
type State = (Source, Tokens)

-- | the lexer monad to be unified all lexing functions. Results in either an error or a state.
type LexerM = Either ErrorMsg State

newtype ErrorMsg = Unterminated String deriving (Show)

-- | TODO creates a total enclosure for all lexing functions.
eat :: State -> LexerM
eat state =
  let
    (Source (x : xs), ts) = state
   in
    case matchChar x of
      WHITESPACE -> eat (Source xs, ts)
      DBL_QUOTE -> poopString state (Just DBL_QUOTE) mempty
      LEFT_BRACE -> poopArray state (Just RIGHT_BRACE) mempty
      LEFT_BRACKET -> poopObjet state (Just RIGHT_BRACKET) mempty

-- | TODO needs to be composed from poopString
poopObjet :: State -> Maybe MonoChar -> [Token] -> LexerM
poopObjet (Source [], _) (Just terminal) _ = Left $ Unterminated $ show terminal
poopObjet state Nothing _ = Right state
poopObjet (Source (x : xs), ts) (Just terminal) obj = undefined -- TODO

-- | TODO needs to be composed from poopString
poopArray :: State -> Maybe MonoChar -> [Token] -> LexerM
poopArray (Source [], _) (Just terminal) _ = Left $ Unterminated $ show terminal
poopArray state Nothing _ = Right state
poopArray (Source (x : xs), ts) (Just terminal) arr = undefined -- TODO

-- | TODO: need to tokenize any integers.
lexNumber = undefined -- TODO

{- | parses a source string continuously until seeing an expected MonoChar Wrap.

-- TODO: check for EOF.

__For example:__

@
input = "abc\\""
reult = poopString input (Just DBL_QUOTE) mempty
result == ("", STRING "abc") -- >>> True
@
-}
poopString :: State -> Maybe MonoChar -> [Char] -> LexerM
poopString (Source [], _) (Just terminal) _ = Left $ Unterminated $ show terminal
poopString state Nothing _ = Right state
poopString (Source (x : xs), ts) (Just terminal) str
  | matchChar x == terminal =
      poopString (Source xs, tokens str) Nothing str
  | otherwise =
      poopString (Source xs, ts) (Just terminal) (x : str)
 where
  tokens [] = terminated : ts
  tokens s = POLY (STRING (reverse s)) : terminated : ts
  terminated = MONO terminal
