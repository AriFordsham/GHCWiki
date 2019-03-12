This page is to track work by @DanielG and @mpickering on making GHC work _perfectly_ for tooling. 

The primary goal is

> It should be possible to load multiple different projects into a single GHC API session

This functionality is important for tooling such as `haskell-ide-engine` which needs to deal with giving feedback to user's on multiple different projects simultaneously. Orchestrating this from a single process is 
simpler and more efficient. 

# Current Situation

# Desired API

# Work to do