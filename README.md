I design and implement a JSON parser in Haskell.

## Design

The JSON parser is composed of at least 2 components: a lexer and a parser.

The Lexer is responsible of scanning the input data (a continuous string) character-by-character. It attempts to map each character to either _a token_ or _an entry point_ into a lexical context so as to produce a more complex token. The Lexer can reduce as much as 80% of the work for the Parser.

Once the input has been tokenized, the list of tokens shall then be passed to the Parser to be transformed into an _Abstract Syntax Tree_ (AST) obeying the grammar and syntax required by the JSON specs.

### The Lexer

The Lexer needs to handle as many edge cases and as early as possible. This wll relieve the Parser to focus only on AST transformation. For example, the Lexer should terminate when it encounters an empty input. Another case is when lexing and tokenizing an unterminated string. A typical JSON string is enclosed in a pair of double quote. For the sake of simplicity, I will handle double quotes only for now. An unterminated string does not close out with the second double quote:

```json
// error
[ "abc ]
```
```json
// correct
[ "abc" ]
```

In this situation, we only need to check for 2 cases: *either* we see a terminal token (a second double quote) *or* we still see nothing by the end of the input file. Thus, we can employ an `Either a b` monad to represent these 2 branches. `Either` affords us informative reports with the `Left` branch on error signals. The Parser should not have to worry about these errors.

However, before we continue, we need to pull back and model a finite automaton (or a finite state machine, FSM) for our Lexer.

$$
FSM = \\{ \Sigma, X, T, Q \\}
$$

where
- $\Sigma$ is the lexicon.
    - neccessarily requires at least one function for mapping between a character to an accepted token.
    - any symbols in `[a-z][A-Z][0-9]` as well as some operator symbols for enclosure and separation.
- $X$ is the input string.
    - the input follows a _consuming model_, i.e. each character symbol is consumed on match. The input is completely *consumed* at the end of tokenization.
    - in C, you will likely use the _indexing model_ to track the positions of various symbols in the input.
- $T$ is the set of output tokens.
    - the length of $T$ is bounded up to the size of input $X$.
- $Q$ is the set of states, reflecting the changes in the input and the list of output tokens.
    - necessarily requires at least a transition function.
- various transition functions, which might commute.

#### Lexicon of Symbols and Tokens

First, we define a lexicon function. We now notice that there are 2 groups of tokens. There are meta-tokens such as the braces, brackets, double quotes, colons and commas. These tokens either delimit or provide boundaries separating the data elements. These tokens are single-point because each is represented by a single character symbol.

On the other hand, there is another group of tokens. I call these multi-point tokens. Each token of this type is composed of a vector of character symbols. For example, both `String "abc"` and `Number 123` are vectors of points. The first is a vector of letters, a monoid to be precise. The second is a vector of digits that is then further collapsed to a valued point in the domain of intergers.

Our lexicon needs to reflect these 2 groups. Then, we define our data types and lexicon as follows:

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

This lexicon is the entry point into our Lexer, filtering input symbols into various tokenizing paths. For more complex data types like `String`, `Array` and `Object` we will need dedicated processor functions. These functions might even need to be defined recursively in terms of one another. For example, an array can be thought as a "chunk" of data elements enclosed in brackets. This "chunk" might contain strings or numbers, which need to be tokenized separately.

The data type `Token` technically can be represented as a data tree, with mutliple prefixes to differentiate the token groups. However, we flatten the data type for the sake of simplicity, with only a single prefix for all variants.

Note that it is also quite possible to use the raw symbols such as `"` or `[` as token processing signals. However, a separate data type is useful in various ways. The type name `Token` is more informative than `Char` when matching for lexing paths. For example, let's say we need to be maximally clear that we have the following JSON array as input:

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

will retain all structural information while disambiguating between the string `"123"` and the integer `123`. This is a powerful feature of the Haskell type system. This will be very helpful when we get to building an AST with the Parser.

#### The `LexerM` Functor

Finally, we define a functor to unify the state transitions among various lexing functions. The goal of this functor is to allow fluid access to the source input as well as the developing list of tokens, while also providing a handling path for any errors that might arise during the lexing process.

```hs
type LexerM = Either ErrorMsg State
```

where

```hs
type State = (Source, Tokens)   -- represents the state Q of the FSM

type Source = [Char]            -- the input string

type Tokens = [Token]           -- the list of tokens to be developed

-- a coproduct type to control various lexing errors
data ErrorMsg
    = EmptyInput
    | Unterminated MetaToken
```

#### The `LexerM` Monad

- TODO: I will get to it when I get to it.
    - required for implementing pattern matching on keywords via the `Alternative` functor.

### The Parser

- TODO: I will get to it when I get to it.

## A Note about Early Optimization

For this project, I am pushing for the model to be as detailed as possible. I aim to make explicit all morphisms (example: opting for the typed lexicon over direct matching on characters).

Once the complete paths have been laid out clearly without any shortcuts, the optimization step naturally follows. Only then, we can analyze and compress certain paths if needed, because the correct paths neccessarily commute.

## Resources

1. [the JSON specifications](https://www.json.org/json-en.html)
2. [CS 411 Compiler Design, Carnegie Melon University](https://www.cs.cmu.edu/~janh/courses/411/17/schedule.html)

