---
title: "Parser Combinators in Ruby"
publish: true
---

Parser Combinators in Ruby
==========================

Parser combinators are a technique common in FP languages for writing parsers - programs
that convert textual input into a data structure native to the language. An ubiquitous
example is a JSON parser, which almost every modern language has. Parser combinators are
small building blocks that you compose together to form larger parsers. They're nice for a
number of reasons, but one is that the parser you end up with is extremely readable -
often it looks very close to the formal grammar of the language you're parsing. This makes
it easy to spot bugs and extend in the future.

I won't give a full introduction to parser combinators here. If you're not familiar with
them, there are a lot of tutorials floating about the internet. I particularly recommend
[Monadic Parser Combinators][mpc] by Graham Hutton and Erik Meijer. If you've written
parsers in the past and found yourself in a bit of a mess of regular expressions and
string munging, then you might consider parser combinators as an alternative. In this post
I'm going to walk through the creation of a simple parser combinator library in Ruby and
use it to build a parser for JSON. The library and JSON parser will each be about 100 LOC.

As an example of what we're aiming for, here's a parser written in the combinator style in
Haskell:

```haskell
parseNumberList = brackets (sepBy comma number)

brackets p = between (string "[") (string "]") p
sepBy sep p = do
  first <- p
  rest <- many (sep *> p)
  pure (first : rest)
comma = string ","
between open close inner = do
  open
  val <- inner
  close
  pure val
number = ...
```

The high level parser `parseNumberList` is constructed from small building blocks,
including several parsers which modify other parsers: these are called _combinators_.
`brackets` parses an open bracket, the given inner parser, and a closing bracket.
`between` is a generic version of `brackets` which runs the three parsers provided in the
same order. Because these parsers are generic we can reuse them in many places. For
example, if we wanted a parser for a two-element tuple like `(1,2)` we might write:

```haskell
tuple = parens do
  fst <- number
  comma
  snd <- number
  pure (fst, snd)
parens p = between (string "(") (string ")") p
```

The end result tends to be a very small amount of code and a very clear description of the
language.

To parse in the combinator style you need two things: first class functions and custom
flow control. In Ruby we'll model these respectively using Procs and exceptions.

I'll briefly describe Procs and exceptions - feel free to skip this part if you're
comfortable with them.

## Procs

Ruby's equivalent of a first class function is the Proc (or lambda):

```ruby
add_one = proc { |x| x + 1 }

> add_one.call(1)
=> 2

id = proc { |x| x }

> id.call("ruby")
=> "ruby"
```

`proc` takes as argument a block, which is the body of the function. Any parameters in the
block become formal parameters to the constructed Proc. Procs have access to any variables
in scope when they are constructed, meaning that they form closures.

```ruby
a = 1
inc_a = proc { a += 1 }
inc_a.call
inc_a.call

> a
=> 3
```

## Exceptions

When we write two sequential statements in a Ruby method, they will be executed in the
usual imperative order.

```ruby
def a
  puts "a"
end

def b
  puts "b"
end

def foo
  a
  b
end

> foo
a
b
=> nil
```

`a` has no way to control whether `b` is executed after it. However if `a` raises
an exception then this changes: control will jump out of the method and any enclosing
methods until we reach a matching `rescue` statement. We can abuse this feature to control
how our parsers behave. Specifically, if a parser is made of several smaller parsers
combined together, and one of the smaller parsers fail, we want the entire parser to fail.
We want to be able to override this when necessary, but this should be the default.

## The Core

Armed with these language features we can start to construct our parser. We'll assume that
the input is always a string, and we'll track our progress through it with an index `loc`
(lazy shorthand for "location").

```ruby
class Parser
  def initialize(input)
    @input = input
    @loc = 0
  end

  # ...
end
```

The intuition is that our parsing methods will "consume" portions of the input, advancing
the index as they do. Subsequent parses will only see the remaining input.

```ruby
def input
  @input.byteslice(@loc..-1)
end

def consume(num)
  @loc += num
end
```

If a parse fails, we want to reset the index back to where it was before we started that
parse. This behaviour is called backtracking, and it isn't the only option in parser
design, but we use it here because I think this behaviour makes it easier to write clear
parsers.

To provide this behaviour we define a method `backtrack`, which takes a block. If that
block fails to parse, `backtrack` will reset the index after it. How do we know if a parse
has failed? We'll use an exception. This has the nice property that if the parser consists
of multiple parts and the first part fails, the subsequent parts will be skipped. Each
parser doesn't need to check if the previous parser succeeded, because if it had failed it
would have raised an exception and we would never reach the second parser. This is what
allows us to write parsers cleanly (as you'll see in a minute) without any plumbing of
checking return values between one and the next.

```ruby
def backtrack
  loc_before = @loc
  yield
rescue ParseFailure
  @loc = loc_before
  # re-raise the exception
  # to propagate the failure
  raise
end
```

These three methods form the core of our parser: everything else can be constructed using
them. First up, let's define the most permissive parser: `take`. `take(n)` will parse `n`
characters from the input. It will only fail if there are fewer than `n` characters left.

```ruby
def take(len)
  match = input.byteslice(0, len)
  if match.length < len
    raise ParseFailure, "expected #{len} characters, but saw #{match.length}"
  end

  consume(len)
  match
end
```

We first fetch `len` characters, raising `ParseFailure` if we don't get enough characters.
We then `consume` the characters we've just fetched, and return them. This is how it
behaves:

```ruby
p = Parser.new("what a great example")
p.take(100)
=> Parser::ParseFailure:
   expected 100 characters, but saw 20
p.take(4)
=> "what"
p.take(2)
=> " a"
```

Next up: string literals.

```ruby
def string(pat)
  backtrack do
    s = take(pat.length)
    fail(pat, s) unless s == pat

    s
  end
end

def fail(expected, actual)
  raise ParseFailure, "expected #{expected.inspect} but saw #{actual.inspect} (#{@loc})"
end
```

`string("abc")` will succeed if the input starts with the string `abc`. To implement it,
we `take` enough characters to match the length of the given string, and then check if
they match. If they don't, we raise `ParseFailure` (via a convenience method `fail`). We
wrap all of this in `backtrack` to ensure that when we do fail, we roll back the index.

Finally, we'll want a way to repeatedly parse characters matching some predicate. This is
`take_while`[^1]:

```ruby
def take_while(pred)
 input.chars.take_while(&pred).join.tap { |match| consume(match.length) }
end
```

We can now write simple parsers, such as this:
```ruby
class PurchaseOrder < Parser
  def run
    string "BUY: "
    take_while proc { |c| c != "\n" }
  end
end

> Order.new("BUY: eggs").run
=> "eggs"
```

## Combinators

To complete our parser, we need ways to compose these primitives together. This is where
we'll meet our combinators. The first is `either`:

```ruby
def either(parser1, parser2)
  backtrack { parser1.call }
rescue ParseFailure
  parser2.call
end
```

`either` takes two parsers as arguments. It tries the first, and if that fails it
backtracks and tries the second.

```ruby
run = proc do |input|
  p = Parser.new(input)
  p.either(
    proc { p.string("foo") },
    proc { p.string("bar") },
  )
end

run.("foo")
=> "foo"
run.("bar")
=> "bar"
run.("cat")
Parser::ParseFailure:
  expected "bar" but saw "cat" (3)
```

Next we have `zero_or_more`:

```ruby
def zero_or_more(parser)
  matches = []
  loop { matches << backtrack { parser.call } }
rescue ParseFailure
  matches
end
```

`zero_or_more` takes a parser and tries to apply it as many times as possible, returning
an array of results. It always succeeds, as the parser can match zero times. This is
analogous to the `*` regex operator.

```ruby
run = proc do |input|
  p = Parser.new(input)
  p.zero_or_more(proc { p.string("a") })
end

run.("a")
=> ["a"]
run.("aaaabb")
=> ["a", "a", "a", "a"]
run.("baaa")
=> []
```

If we want the behaviour of regex `+`, we can use `at_least_one`:

```ruby
def at_least_one(parser)
  first = parser.call
  rest = zero_or_more(parser)
  [first, *rest]
end
```

`at_least_one` is identical to `zero_or_more` except that the parser must succeed at least
once.

The last combinator we'll look at is `sep_by`, which is the same as `sepBy` in the Haskell
example from the start. Given a separator and a parser, it will try to parse repeated
instances of the parser interspersed with the separator (which is itself a parser).

```ruby
def sep_by(separator, parser)
  first = begin
            backtrack { parser.call }
          rescue ParseFailure
            return []
          end

  combined = proc do
    separator.call
    parser.call
  end
  rest = zero_or_more combined
  [first, *rest]
end
```

```ruby
run = proc do |input|
  p = Parser.new(input)
  p.sep_by(
    proc { p.string(",") },
    proc { p.take(1) },
  )
end

run.("")
=> []
run.("1,2,3")
=> ["1", "2", "3"]
run.("1,2,3,45")
=> ["1", "2", "3", "4"]
```

That's basically it! There are a couple more combinators we could define, like `optional`,
`one_of` and `between`, but they are straightforward. Let's look instead at writing a real
world parser with this tooling.

## Parsing JSON

We're going to write a mostly-compliant JSON parser. It won't handle unicode and its
floating point behaviour will be a bit broken, but it will otherwise work correctly. I've
tested it against the fixtures in the [JSON Parsing Test Suite](https://github.com/nst/JSONTestSuite/)
and it passes 109 of the 141 `y_` tests (samples that must be accepted by the parser). In
total, the whole parser is 119 lines long.

To start, we define our class and entrypoint

```ruby
class JsonParser < Parser
  def run
    json_value
  end
```

`json_value` parses, well, any JSON value.

```ruby
def json_value
  one_of [
    method(:object),
    method(:array),
    method(:quoted_string),
    method(:boolean),
    method(:null),
    method(:number),
  ]
end
```

`one_of` is a combinator that acts like a variadic `either`: it will try each parser in
turn until one succeeds. Each of the parsers given to it is a method on our class, so we
convert them to first class objects using
[`Object#method`](https://ruby-doc.org/core-2.6/Object.html#method-i-method).  This will
allow us to `#call` them just like Procs. We'll walk through each of these methods in
turn.

```ruby
def object
  inner = proc do
    kvs = sep_by method(:comma), method(:key_value_pair)
    Hash[kvs]
  end
  between proc { token "{" },
          proc { token "}" },
          inner
end

def token(str)
  string(str).tap { skip_spaces }
end

def skip_spaces
  take_while(proc { |c| [" ", "\n"].include?(c) })
end
```

`object` uses `between` to parse the opening and closing brackets. Inside the brackets we
parse a series of key-value pairs. We use `sep_by` with a separator of `comma` (which does
what you'd expect), and an inner parser of `key_value_pair`. This gives us a nested array
which we convert to a Hash before returning. To parse the brackets we use `token`, which
is a wrapper around `string` which consumes any trailing whitespace. We'll use
it throughout the parser - it allows us to largely ignore whitespace and keep
things concise.

```ruby
def key_value_pair
  key = quoted_string
  token ":"
  value = json_value
  [key, value]
end

def quoted_string
  str = between proc { string "\"" },
                proc { token "\"" },
                proc { take_while(proc { |c| c != "\"" }) }
  str
end
```

`key_value_pair` is a string enclosed in quotes followed by a semicolon, followed by any
JSON value. We naïvely assume that a string is any sequence of characters excluding the
double quote character. This obviously doesn't cater for escape characters, but we'll
conveniently ignore that. Note that we can't use `token` for the opening quote in
`quoted_string` because any trailing whitespace after that character forms part of the
string we're trying to parse.

That's JSON objects, then. Next up, arrays:

```ruby
def array
  res = between proc { token "[" },
                proc { token "]" },
                proc { sep_by method(:comma), method(:json_value) }
  res
end
```

We again use `between` to handle the enclosing brackets, and inside them we just parse a
series of JSON values separated by commas.

Booleans and `null`s are very simple:

```ruby
def boolean
  bool = either proc { token "true" }, proc { token "false" }
  bool == "true"
end

def null
  token "null"
  nil
end
```

And all that's left is numbers. JSON has one number type, which is the float. Parsing
floats is a bit complex as we have to deal with positive/negative signs, decimals and
exponents, but we can cater for all of that without a huge amount of code. It is, however,
a little less clear than what we've covered so far. I'll leave it as an exercise to work
out what we're doing here - hopefully it's not too hard!

```ruby
def number
  sign = optional method(:sign)
  result = integer
  decimals = optional(proc { string "."; integer })
  exponent = optional(proc do
    either(proc { string "e" }, proc { string "E" })
    signed_integer
  end)

  result = result.to_f if decimals || exponent
  result += (decimals.to_f / (10**decimals.to_s.length)) if decimals
  result *= 10**exponent.to_f if exponent
  sign == "-" ? 0 - result : result
end

def signed_integer
  sign = optional method(:sign)
  n = integer
  sign == "-" ? 0 - n : n
end

INTEGERS = (0..9).map(&:to_s)
def integer
  int = proc do
    char = take 1
    fail("one of #{INTEGERS}", char) unless INTEGERS.include?(char)

    char
  end

  numstr = at_least_one int
  skip_spaces
  numstr.join.to_i
end

def sign
  either proc { token "-" },
         proc { token "+" }
end
```

That's the whole thing. Let's try it out on some JSON.

```ruby
JsonParser.new("{}").run
=> {}

JsonParser.new("[]").run
=> []

JsonParser.new("4").run
=> 4

JsonParser.new(<<EOF).run
{
  "a":   "sample",
  "json"  : "object"
}
EOF
=> {"a"=>"sample", "json"=>"object"}

JsonParser.new(<<EOF).run
{
  "a": "sample",
  "json": "object",
  "with": [
    "an",
    "array",
    -1.23e3,
    { "two": "three" }
  ]
}
EOF
=> {
     "a" => "sample",
     "json" => "object",
     "with" => [
       "an",
       "array",
       -1230.0,
       { "two" => "three" }
      ]
   }

JsonParser.new("bad input").run
=> Parser::ParseFailure: expected one of: [
  #<Method: JsonParser#object>,
  #<Method: JsonParser#array>,
  #<Method: JsonParser#quoted_string>,
  #<Method: JsonParser#boolean>,
  #<Method: JsonParser#null>,
  #<Method: JsonParser#number>
]
from lib/parser.rb:60:in `one_of'
```

And that's it! A (mostly) complete JSON parser in around 100 LOC. Hopefully that gives you
an idea of how you can take these parsing techniques and apply them outside of functional
programming languages.

## Performance

There's one final thing to address, and that is performance. Ruby isn't known for its
speed, and for tasks like this it often delegates to an underlying C library to do the
heavy lifting - this is the case for most JSON, YAML and XML parsers. The parser we've
written here is, comparatively, extremely slow: parsing a 100KB JSON document on my laptop
takes 3.5 seconds compared to ~0.15 seconds using the Ruby standard library JSON
parser[^2]. I'm not yet sure whether this is due to slow string indexing, repeated
backtracking, overuse of exceptions, or something else - it would be quite interesting to
find out. Until then, just keep in mind that whilst these techniques do translate, the
tradeoffs might be very different between languages!

You can find all the code for this post [here](https://github.com/hmac/rubyparsers).

[^1]: A more idiomatic definition of `take_while` would take a block argument rather than
  a proc, so that you can write `take_while { |c| ... }`. I've not done this here to keep
  the usage of Procs consistent, but you could certainly do so in the real world.
[^2]: To be specific, the [`ext`][ext] implementation, which is 2000 lines of C, rather than the
  pure Ruby implementation.

[mpc]: http://eprints.nottingham.ac.uk/237/1/monparsing.pdf
[ext]: https://github.com/ruby/ruby/blob/trunk/ext/json/parser/parser.c
