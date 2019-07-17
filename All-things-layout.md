This is a brainstorm page to see how far we can push our love for layout-based syntax to fix existing problems with lists, records, multi-line strings etc.

This might just stay a vanity idea, or evolve into a proper extensions.

## Herald

I (@nomeata) don’t have good idea for the layout herald yet. So I am using a new keyword `layout`, followed by what kind of things follow (`layout list`, `layout string` etc.) I actually find this the hardest question.

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

== Strings

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