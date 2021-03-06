# Notes for using Haddock (for GHC)


Some comments use different "markup" than Haddock.  Consult the [Haddock Markup Rules](http://www.haskell.org/haddock/doc/html/ch03s08.html).

## Haddock Pitfalls


Haddock is not very forgiving, so the following is a list of issues and suggested workarounds.  Items in **bold** are problems where the **error location does not show the actual problem** and are thus hard to spot.  


Note: All problems below were encountered with Haddock 0.9, different versions may behave differently.

- **Trailing `where` clause**.  If a function ends with an empty `where` clause, Haddock will consider all remaining functions (regardless of indentation level) as local functions.  If you're lucky it fails at the next `type` or `data` declaration, if you're not lucky, the remaining functions will probably be silently ignored.  So, be careful!

- **Bullet lists (`*`) in module head**.  If your module description contains not properly indented bullet lists, Haddock will fail with a parse error at the `module Foo` part.  The workaround is to use `-` instead of `*`.

- **Trailing semicolon in `case` expression**.  Given the following code

  ```wiki
  do { foo <- calcSomeFoo
     ; case foo of
         Xxx -> blahBlah ;  -- <-- HERE
         Yyy -> ueouoe
     }
  ```

  Haddock will choke after the `Yyy` constructor.  The problem is the trailing semicolon in the previous line.

- **`do` with bracket on next line**.  In the following code

  ```wiki
    blah foo $ do
    { blah blub
    ; ...
    }
  ```

  Haddock does not know that the opening brace belongs to the `do`.  Move it after the `do` and you're fine.

- **`\end{code`} with trailing whitespace**.  This will fail somewhere trying to parse the documentation.

- Emphasis markers look like markup.  If a word is emphasised using the common `*important*` meme, Haddock gets confused when such a word appears at the beginning of a comment line.  If you want to convert the comment to Haddock markup you can use haddocks emphasising mechanism `/important/`.  If it should remain a comment, you can use underscores `_important_`.

- Constructors with multiple unnamed arguments.  Haddock doe not understand the following

  ```wiki
  data Foo = A    -- ^ describe constructor
              Arg1 -- ^ describe first argument
              Arg2 -- ^ and this
  ```

  The workarounds are to use named fields or to refer to arguments by number like this:

  ```wiki
  data Foo = A Arg1 Arg2
               -- ^ describe constructor.  Parameters:
               --
               --   1. describe first argument
               --
               --   2. describe second argument
  ```

  Note that the newlines are mandatory, otherwise the list will be inline which is a lot harder to read.

- Tuple components.  As above, you cannot document of a function parameter/result.

- Preprocessor macros.  Some preprocessor macros do not expand to something Haddock can parse.  
  You can define simpler substitutions by testing the `__HADDOCK__` variable:

  ```wiki
  #ifndef __HADDOCK__
  ... default definition ...
  #else
  ... simplified definition ...
  #endif
  ```