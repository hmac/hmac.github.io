---
title: "Parser Combinators in Ruby"
publish: false
---

Parser Combinators in Ruby
==========================

I'm a big fan of parser combinators as a parsing technique. They're relatively easy to use
and produce very readable parsing code. For the most part you end up with a parser that
looks a lot like the equivalent BNF definition.

Parser combinators are most often seen in functional languages, as they make heavy use of
first class functions and monadic flow control. However we can emulate them in other
languages, with a bit of work. I tried this recently with Ruby and the result was quite
nice. What follows is a walkthrough of a Ruby parser combinator library in around 100 LOC.

As an example of what we're aiming for, here's an example of a parser written in the
combinator style in Haskell:

```haskell
parseNumberList = brackets (sepBy comma number)

brackets p = between "[" "]" p
sepBy sep p = do first <- p
                 rest <- many (sep *> p)
                 pure (first : rest)
comma = string ","
number = ...
```

The high level parser `parseNumberList` is constructed from small building blocks,
including several parsers which _modify other parsers_: combinators. `brackets` parses an open bracket,
the given inner parser, and a closing bracket. Because these parsers are generic we can
reuse them in many places. The end result tends to be a very small amount of code and a
very clear description of the language.

To parse in the combinator style you need two things: first class functions and custom
flow control. In Ruby we'll model these respectively using Procs and exceptions.

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
def do_a
  puts "a"
end

def do_b
  puts "b"
end

def foo
  do_a
  do_b
end

> foo()
a
b
=> nil
```

`do_a` has no way to control whether `do_b` is executed after it. However if `do_a` raises
an exception then this changes: control will jump out of the method and any enclosing
methods until we reach a matching `rescue` statement. We can abuse this feature to control
how our parsers behave.

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
of multiple parts and the first part fails, the subsequent parts will be skipped.

```ruby
def backtrack
  loc_before = @loc
  yield
rescue ParseFailure
  @loc = loc_before
  raise             # re-raise the exception, as we want to propagate the failure
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
We then `consume` the characters we've just fetched, and return them.

Next up: string literals.

```ruby
def string(pat)
  backtrack do
    s = take(pat.length)
    fail(s, pat) unless s == pat

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
`take_while`:

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
we'll meet our first combinators. The first is `either`:

```ruby
def either(parser1, parser2)
  backtrack { parser1.call }
rescue ParseFailure
  parser2.call
end
```

`either` takes two parsers as arguments. It tries the first, and if that fails it
backtracks and tries the second. Next we have `zero_or_more`:

```ruby
def zero_or_more(parser)
  matches = []
  loop { matches << backtrack { parser.call } }
  matches
rescue ParseFailure
  matches
end
```

`zero_or_more` takes a parser and tries to apply it as many times as possible, returning
an array of results. It always succeeds, as the parser can match zero times. This is
analogous to the `*` regex operator. If we want the behaviour of regex `+`, we can use
`at_least_one`:

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

That's basically it! There are a couple more combinators we could define, like `optional`,
`one_of` and `between`, but they are straightforward. Let's look instead at writing a real
world parser with this tooling.

## Parsing JSON

We're going to write a mostly-compliant JSON parser. It won't handle unicode and its
floating point behaviour will be a bit broken, but it will otherwise work correctly. I've
tested it against the fixtures in the [JSON Parsing Test Suite](https://github.com/nst/JSONTestSuite/)
and it passes 109 of the 141 `y_` tests (samples that must be accepted by the parser). In
total, the whole parser is 109 lines long.

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
  skip_spaces
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

There's a couple of things to unpick here. Firstly, `skip_spaces` is a parser that will
skip any leading whitespace (spaces or newlines) from the input. It is defined as follows:

```ruby
def skip_spaces
  take_while(proc { |c| [" ", "\n"].include?(c) })
end
```

Secondly, `one_of` is a combinator that acts like a variadic `either`: it will try each
parser in turn until one succeeds. Each of the parsers given to it is a method on our
class, so we convert them to first class objects using [`Object#method`](https://ruby-doc.org/core-2.6/Object.html#method-i-method).
This will allow us to `#call` them just like Procs. We'll walk through each of these
methods in turn.

```ruby
def object
  inner = proc do
    skip_spaces
    kvs = sep_by method(:comma), method(:key_value_pair)
    skip_spaces
    Hash[kvs]
  end
  between proc { string "{" },
          proc { string "}" },
          inner
end
```

`object` uses `between` to parse opening and closing brackets. Inside the brackets we
parse a series of key-value pairs, separated by optional whitespace. We use `sep_by` with
a separator of `comma` (which does what you'd expect), and an inner parser of
`key_value_pair`. This gives us a nested array which we convert to a Hash before
returning.

```ruby
def key_value_pair
  key = quoted_string
  string ":"
  skip_spaces
  value = json_value
  [key, value]
end

def quoted_string
  between proc { string "\"" },
          proc { string "\"" },
          proc { take_while(proc { |c| c != "\"" }) }
end
```

`key_value_pair` is a string enclosed in quotes followed by a semicolon, followed by any
JSON value. We naïvely assume that a string is any sequence of characters excluding the
double quote character. This obviously doesn't cater for escape characters, but we'll
conveniently ignore that.

That's JSON objects, then. Next up, arrays:

```ruby
def array
  res = between proc { string "["; skip_spaces },
                proc { skip_spaces; string "]" },
                proc { sep_by method(:comma), method(:json_value) }
  skip_spaces
  res
end
```

We again use `between` to handle the enclosing brackets, and `skip_spaces` for whitespace.
I've put the calls to `skip_spaces` inside the bracket parsers this time but there's no
particular reason for that except to keep the inner parser short enough to inline. Inside
the brackets, we just parse a series of JSON values separated by commas.

Booleans and `null`s are very simple:

```ruby
def boolean
  bool = either proc { string "true" }, proc { string "false" }
  bool == "true"
end

def null
  string "null"
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
  result = integer
  decimals = optional(proc { string "."; integer })
  exponent = optional(proc { either(proc { string "e" }, proc { string "E" }); integer })

  result = result.to_f if decimals || exponent
  result += (decimals.to_f / (10**decimals.to_s.length)) if decimals
  result *= 10**exponent.to_f if exponent
  result
end

INTEGERS = (0..9).map(&:to_s)
def integer
  int = proc do
    char = take 1
    fail("one of #{INTEGERS}", char) unless INTEGERS.include?(char)

    char
  end

  sign = optional method(:sign)
  numstr = at_least_one int
  n = numstr.join("").to_i
  sign == "-" ? 0 - n : n
end

def sign
  either proc { string "-" },
         proc { string "+" }
end
```

And that's it! A (mostly) complete JSON parser in around 100 LOC. Hopefully that gives you
an idea of how you can take these parsing techniques and apply them outside of functional
programming languages.

There's one final thing to address, and that is performance. Ruby isn't known for its
speed, and for tasks like this it often delegates to an underlying C library to do the
heavy lifting - this is the case for most JSON, YAML and XML parsers. The parser we've
written here is, comparatively, extremely slow: parsing a 100KB JSON document on my laptop
takes 3.5 seconds. I'm not yet sure whether this is due to slow string indexing, repeated
backtracking, overuse of exceptions, or something else - it would be quite interesting to
find out. Until then, just keep in mind that whilst these techniques do translate, the
tradeoffs might be very different between languages!

You can find all the code for this post [here](https://github.com/hmac/rubyparsers).
