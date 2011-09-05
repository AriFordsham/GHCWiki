
Many packages these days are using Template Haskell to grab information from the filesystem at compile time. I'll use Hamlet as a motivating example, though this applies to others as well.


In Hamlet, you can specify an external file as the source for your HTML template code, with something like:

> `renderHamlet $(hamletFile "myfile.hamlet")`


Unfortunately, if "myfile.hamlet" is updated, without making any changes to the Haskell code, then GHC will not know to recompile the source, and the updated HTML code will not filter through.


To solve this, a [ possible solution](http://www.reddit.com/r/haskell/comments/k4lc4/yesod_the_limitations_of_haskell/c2hipo3) is to add a new function to the template-haskell package:

> `addDependentFile :: FilePath -> Q ()`


When GHC encounters this, it will add the specified file as a dependency for the given Haskell file. If the file is updated or is not present, then a recompile will be required.
