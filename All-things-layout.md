This is a brainstorm page to see how far we can push our love for layout-based syntax to fix existing problems with lists, records, multi-line strings etc.

This might just stay a vanity idea, or evolve into a proper extension.

# Herald

I (@nomeata) don’t have good idea for the layout herald yet. So I am using a new keyword `layout`, followed by what kind of things follow (`layout list`, `layout string` etc.) I actually find this the hardest question.

For things that replace enumerations (e.g. `(…)`, `{…}`, `[…]`), having the opening symbol at the end of the line might work as a herald, but then you kinda also want the closing one, which is not how layout syntax works in Haskell in general (e.g. you don't need to close a `where` or a `do`).

## Lists

```
planets :: [String]
planets = layout list
    "Mercury"
    "Venus"
    "Earth"
    "Mars"
    "Jup" ++ "iter"
    "Saturn"
    reverse
       "sunarU"
    "Neptune"
```
desugars to 
```
planets :: [String]
planets =
    [ "Mercury"
    , "Venus"
    , "Earth"
    , "Mars"
    , "Jup" ++ "iter"
    , "Saturn"
    , reverse
        "sunarU"
    , "Neptune"
    ]
```

## Tuples

```
triple :: (String, Int, [String])
triple = layout tuple
  "Foo"
  23 + 19
  planets
```
desugars to
```
triple :: (String, Int, [String])
triple = ("Foo", 23 + 19, planets)
```

The tuple syntax is used in more places than just term or type tuples, e.g. import and export list. These could have their own herald `layout import`. 

## Strings

Essentially, here-documents (with indentation removed!)

```
lorem :: String
lorem = layout string
    Lorem ipsum dolor sit amet, consectetur
    adipiscing elit, sed do eiusmod tempor
    incididunt ut labore et dolore magna aliqua
```
desguars to 
```
lorem :: String
lorem = "\
    \Lorem ipsum dolor sit amet, consectetur\n\
    \adipiscing elit, sed do eiusmod tempor\n\
    \incididunt ut labore et dolore magna aliqua\n"
```


## Records 

Also see https://github.com/ghc-proposals/ghc-proposals/pull/231 which proposes precisely that, with a `with` keyword.

```
data BigRec = BigRec layout record
  field1 :: Int
  field2 :: Bool
  field3 :: String

Rec layout record
  field1 = 1
  field2 = True

r layout record
  field1 = field1 r + 1
  field2 = True
```
desugars to
```
data BigRec = BigRec
  { field1 :: Int
  , field2 :: Bool
  , field3 :: String
  }

BigRec
  { field1 = 1
  , field2 = True
  , field3 = "hello"
  }

r { field1 = field1 r + 1
  , field2 = True
  }
```

## Arguments

```
complex_function (short argument) layout arguments
  first long argument
  second long argument
  foo layout argument
    bar
    baz
  quuz
```
desugars to
```
complex_function (short argument)
  (first long argument)
  (second long argument)
  (foo bar baz)
  quuz
```  


# Comments

* Feel free to add comments here. Also of course feel free to brainstorm alternatives, variants etc.

## Herald idea: Backslash+symbol

Author: @int-index

```
planets :: [String]
planets =
  \[ "Mercury"
     "Venus"
     "Earth"
     "Mars"
     "Jup" ++ "iter"
     "Saturn"
     reverse
       "sunarU"
     "Neptune"

triple :: (String, Int, [String])
triple =
  \( "Foo"
     23 + 19
     planets

lorem :: String
lorem =
  \" Lorem ipsum dolor sit amet, consectetur
     adipiscing elit, sed do eiusmod tempor
     incididunt ut labore et dolore magna aliqua

data BigRec =
  BigRec \{
    field1 :: Int
    field2 :: Bool
    field3 :: String

Rec \{
  field1 = 1
  field2 = True

r \{
  field1 = field1 r + 1
  field2 = True

complex_function (short argument) \$
  first long argument
  second long argument
  foo \$
    bar
    baz
  quuz
```


Advantages:

* reuse of existing symbols: `[` for lists, `(` for tuples, `"` for strings, `{` for records, and `$` for function application, so is does not take long to figure out which herald does what
* short! just two symbols

Stolen syntax:

* the `\$` operator
* unit lambda, `:t \( ) -> ()`

### comments

* Neat! It might even not confuse syntax highlighers that there is no closing parenthesis if they consider the `\` as escaping.. I am still a bit worried about editors getting horribly confused.   --@nomeata

* This is really cool! Of the stolen syntax, I'm not worried about the operator; Stackage Hoogle doesn't show any instances of `\$`. But lambda is definitely a concern, and not just for unit: also lambdas with partial matches on empty list `\[ ]` and empty/whitespace-only string `\" "`. Maybe have the parser look ahead to the next non-whitespace character, and if it's the appropriate closing brace don't enter block mode? Though _that_ might break on `\" ""` (first character in an attempted layout string is a double-quote).  
I'm also a bit concerned about your `\"` example. How is the whitespace managed, there? If that were a normal string, the space after `\"` would be part of the string. If that's desired behavior, I think it implies that each subsequent line also begins with a space (because the block's indentation point is just past the `\"`, not under the `L`. If it's _not_ desired (i.e. first character of the string should be `L`), are we implicitly adding a space to the _end_ of each line? How do we know that's the correct separator? And what if the herald is on the previous line (my preferred style) but you also want a leading space? I believe that's impossible, since the first line sets the block indentation and the second can't be to its left. (Thinking about it, most of these problems apply to any of the strategies here; I'm not sure strings are a good fit for layout at all. Excluding them seems to make everything a lot easier.)   --@trac-tejon

## context + `with`

Another idea:

```
-- record creation or pattern
Foo{..} with
  x = 3
  y = 'a'

-- record definition
data Foo = Foo{..} with
  x :: Int
  y :: Char

-- list
x = [..] with
  a
  b
  c

-- tuple
y = (..) with
  a
  b
  c

-- record update
z = f{..} with
  x = 3
  y = 4

-- prefix let
m = f x y with
  x = g 3
  y = case x of ...
```

Advantages:
* Looks nice
* Covers prefix `let` and the other forms
* `with` is always a layout herald, easy for the lexer
* `..` is already a token

Disadvantages:
* ambiguity with RecordWildCards: `Foo{..} with ...` could be a prefix-let or record creation.
* `with` is a big keyword to steal

