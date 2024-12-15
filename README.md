I design and implement a JSON parser in Haskell.

## Design

The JSON parser is composed of at least 2 components: a lexer and parser.

The Lexer is responsible of scanning the input data (a continuous string) character-by-character. It attempts to map each character to _a token_, or _an entry point_ to a lexical context that eventual produces a more complex token. The Lexer can reduce up to 80% of the work for the Parser.

Once the input has been tokenized, the list of tokens shall then be passed to the Parser for complete transformation into an _Abstract Syntax Tree_ (AST) obeying the grammar and syntax required by the JSON specs.

### The Lexer

The Lexer also needs to handle as many edge cases as early as possible. For example, the Lexer should terminate when encountering an empty input. Another case is when lexing and tokenizing an unterminated string. A typical JSON string is enclosed in a pair of double quote. For the sake of simplicity, I will handle double quotes only. An unterminated string does not close out with the second double quote.

```json
// error
[ "abc ]
```
```json
// correct
[ "abc" ]
```

In this case we only need to check for 2 cases: either we see a terminal token (a second double quote) or we see nothing at the end of the input file. We can simply employ an `Either a b` monad to represent these 2 branches. We can afford informative reports in case of errors with the `Left` branch.

However, before we continue, we need to pull back and define a complete finite automaton (FSM) for the Lexer.

$$
FSM = \{ \Sigma, Q, T \}
$$

where
- $\Sigma$ is the lexicon, or the lookup table for mapping between a character to an accepted token.
- $Q$ is the set of states of the state machine.
- $T$ is the set of output tokens.

#### Lexicon of Symbols and Tokens

First, we define a lexicon function. We notice that there are 2 groups of tokens. Meta-tokens such as the braces, brackets, double quotes, colons and commas. These are single-point tokens because they are represented by a single character symbol.

On the other hand, there are another group of tokens in which each is composed of a vector of character symbols. For example, both a `String "abc"` and a `Number 123` are vectors of points. The first is a vector of letters, a monoid to be precise. The second is a vector of digits that is further collapsed to a valued point in the Integer domain.

Therefore, our lexicon needs to reflect these 2 groups. Then, we define our data types and lexicon as follows:

```hs
data MetaToken = DOUBLE_QUOTE | LEFT_BRACE | RIGHT_BRACE | LEFT_BRACKET | RIGHT_BRACKET | WHITESPACE | COMMA | COLON | OTHER

data Token
    = META MetaToken
    | STRING String
    | NUMBER Integer
    | BOOLEAN Bool

lexicon :: Char -> Token
lexicon = case _ of
  '"' -> DBL_QUOTE
  '[' -> LEFT_BRACKET
  ']' -> RIGHT_BRACKET
  '{' -> LEFT_BRACE
  '}' -> RIGHT_BRACE
  ' ' -> WHITESPACE
  ',' -> COMMA
  ':' -> COLON
  _   -> OTHER
```

This lexicon is the first entry point into our Lexer, filtering matching symbols to various tokenizing paths. For complex data types like `String`, `Array` and `Object` we need dedicated processor functions.

It is also quite possible to use the raw symbols such as `"` or `[` as token processing signals. However, a separate data type is useful in various ways. The type name `Token` is more informative than `Char`. For example, let's say we need to be maximally clear that we have an input array:

```json
[ "123", 123 ]
```

Then the following monoid:

```hs
[ META LEFT_BRACKET
, STRING "123"
, META COMMA
, NUMBER 123
, META RIGHT_BRACKET
]
```

will retain all structural information while disambiguating between the string `"123"` and the integer `123`. This will be very helpful when we get to building an AST at the Parser. It's a powerful feature of the Haskell type system.

Finally, we define a monad to unify the state transitions from various lexical functions. The goal of this monad is to allow fluid access to the source input and the developing list of tokens, as well as to handle any errors that arise during the lexing process.

```hs
type LexerM = Either ErrorMsg State
```

where

```hs
type State = (Source, Tokens)   -- represents the state Q of the FSM

type Source = [Char]            -- input string

type Tokens = [Token]           -- list of tokens to be developed

-- a coproduct type to control various lexing errors
data ErrorMsg
    = EmptyInput
    | Unterminated Token
```

### The Parser


## A Note about Early Optimization

For this project, I am pushing for the model to be as detailed as possible. I aim to make explicit all morphisms (example: opting for the typed lexicon over direct matching on characters).

Once the complete paths have been laid out clearly without any shortcuts, the optimization step naturally follows. Then, we can analyze and compress certain paths if needed, because the correct paths neccessarily commute.

## Resources

1. [the JSON specifications](https://www.json.org/json-en.html)
2. [CS 411 Compiler Design, Carnegie Melon University](https://www.cs.cmu.edu/~janh/courses/411/17/schedule.html)

