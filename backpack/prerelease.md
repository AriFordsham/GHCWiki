## Examples

### Hello World


To test that everything is working properly, create the following two files:

```wiki
-- hello.bkp
executable hello where
  main-is: hello

-- hello.hs
main = putStrLn "Hello world!"
```


and then compile with:

```wiki
ghc --backpack hello.bkp
./hello
-- Hello world!
```