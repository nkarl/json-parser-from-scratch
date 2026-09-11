# Learning Retrospective

## Scope

This document reflects on the cognitive and technical stumbling blocks visible in the repository's code, tests, documentation, and development history. These are hypotheses about the learning process represented by the project, not clinical judgments or fixed conclusions about the author.

## Central difficulty

The main difficulty was attempting to discover the architecture, formalize it, learn Haskell's abstractions, and implement JSON correctly at the same time. Each of these is a substantial learning problem on its own. Combined, they made local implementation decisions carry too much conceptual weight.

The project did not stall because JSON parsing was necessarily beyond reach. It stalled because the design was being theoretically justified before a small working implementation had produced enough evidence to show which abstractions were actually useful.

## Inferred stumbling blocks

### Abstraction preceded executable behavior

The README explores monoids, coproducts, morphisms, functors, monads, `Alternative`, finite-state machines, and commuting paths. Those ideas are not inherently inappropriate, but they appeared before the lexer had established its simplest operational loop:

```text
inspect the next character
→ consume it
→ emit zero or one tokens
→ continue with the remainder
```

Without that concrete loop working end to end, it was difficult to evaluate whether each abstraction simplified the implementation or merely renamed part of the problem.

### State was explicit but distributed across too many arguments

The core state types were reasonable:

```haskell
type State = (Source, Tokens)
type LexerM = Either ErrorMsg State
```

The specialized functions also received terminal tokens, predicates, and local accumulators, however:

```haskell
lexNumber :: State -> Maybe MetaToken -> (MetaToken -> Bool) -> [Char] -> LexerM
```

This signature requires the caller to maintain several coupled invariants that the types do not enforce. A smaller initial contract would have isolated the essential behavior:

```haskell
lexString :: String -> Either LexError (String, String)
```

Such a function has one clear responsibility: consume a string token and return both its value and the unconsumed input.

### Input-consumption ownership was unclear

The opening-quote behavior reveals uncertainty about which function owns each character. `runLexer` recognizes a quote but passes the original input, including that quote, to `lexString`. The specialized lexer then immediately recognizes the opening quote as its closing quote.

The relevant contracts had not yet been made explicit:

- Does the dispatcher consume the opening delimiter?
- Does the specialized lexer consume it?
- Which function emits delimiter tokens?
- Which function resumes lexing the remaining input?

A plain-language invariant would have prevented much of this ambiguity:

> `lexString` is called after the opening quote has been consumed. It consumes the closing quote and returns the remaining source.

### Lexical and syntactic responsibilities became entangled

Strings and numbers require lexical scanning, but arrays and objects are recursive syntactic structures composed of tokens. The proposed `lexArray` and `lexObject` functions made the lexer responsible for coordinating nested JSON structures, blurring the boundary the project was trying to establish.

A lexer can remain unaware of whether an array is syntactically valid and emit only a flat stream:

```haskell
[ TokLeftBracket
, TokString "abc"
, TokComma
, TokNumber 123
, TokRightBracket
]
```

The parser can then decide whether those tokens form a valid array. Keeping that boundary firm removes recursion and structural validation from the lexer.

### High-level reasoning competed with language fundamentals

The following clause was intended to recognize an empty source:

```haskell
runLexer (Source mempty, []) = Left EmptyInput
```

In a Haskell pattern, however, a lowercase name introduces a new variable. Here, `mempty` matches every `Source` value rather than only an empty one, so every fresh lexer invocation returns `EmptyInput`. The intended pattern was:

```haskell
runLexer (Source [], []) = Left EmptyInput
```

This is a language-mechanics trap rather than an architectural failure. It nevertheless illustrates the cost of reasoning about advanced abstractions while foundational syntax and evaluation behavior are not yet automatic.

### Type-class generalization arrived before a need for polymorphism

The notes propose implementing `Functor`, `Monad`, and `Alternative` for `LexerM`, but `LexerM` is a type synonym for an `Either` result, and `Either` already supplies the relevant standard instances. More importantly, the alias represents a completed result rather than a computation that consumes lexer state.

A genuinely state-consuming abstraction might eventually look like:

```haskell
newtype Lexer a = Lexer
  { runLexer :: Source -> Either LexError (a, Source)
  }
```

Even this abstraction is best introduced only after several plain functions reveal common composition or state-threading behavior. Until then, direct recursion makes control flow and consumption easier to inspect.

### Tests served as experiments rather than a regression suite

Tests named `testAlwaysFail0` show that the suite was being used as a learning scratchpad. That is useful during exploration, but deliberately failing cases prevent the suite from answering its most important ongoing question: did the latest change break previously working behavior?

The historical tests also use Lens traversals to inspect `Either`, adding an abstraction layer around simple expectations. Comparing complete results would make control flow and errors more visible:

```haskell
assertEqual
  "empty input"
  (Left EmptyInput)
  (runLexer (Source "", []))
```

### The MVP was not bounded tightly enough

"Build a JSON parser from scratch" contains several distinct projects: position tracking, string escapes, Unicode, number grammar, tokenization, recursive parsing, AST design, error reporting, testing infrastructure, and a command-line interface. The repository was also being used to learn Cabal, Nix, Haskell types, monadic composition, and compiler theory.

Without a deliberately restricted finish line, each new discovery expanded the project instead of moving it closer to completion.

## A more suitable learning sequence

1. Write a total lexer using plain recursive functions.
2. Initially support punctuation, unescaped strings, integers, booleans, and `null`.
3. State each function's input-consumption contract explicitly.
4. Keep arrays and objects entirely out of the lexer.
5. Add one passing test for each behavior and remove intentional failures from the normal suite.
6. Build a small recursive-descent parser over the token stream.
7. Define a JSON AST only as detailed as the supported subset requires.
8. After the end-to-end path works, identify repeated patterns that justify introducing a parser type, `State`, `Functor`, `Applicative`, or `Alternative`.
9. Add full JSON details such as escapes, Unicode, fractional and exponential numbers, source positions, and improved diagnostics incrementally.

## Overall assessment

The instinct to model the domain with types was productive. The main adjustment is temporal: use types early to make important states and errors visible, but postpone generalized abstractions until concrete duplication or composition pressure appears.

The project's strongest next step is therefore not a more sophisticated architecture. It is a deliberately small, total, executable path from input text to a JSON value. Once that exists, the architecture can grow in response to observed needs rather than anticipated complexity.
