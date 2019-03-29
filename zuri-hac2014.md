# Bug squashing at ZuriHac2014


Joachim (nomeata) wants to run a small bugsquashing sprint at [ZuriHac 2014](http://www.haskell.org/haskellwiki/ZuriHac2014/Projects). 

## Requirements


You should bring some Haskell experience and be confident reading other people’s Haskell code. You do not need to know all the latest fancy type hackery – GHC itself is written in quite plain Haskell. Some knowledge of git is also useful.


Obviously, you need a machine to work on. The more core it has, the less you’ll have to wait.

## Setup


If you want to join in, you can come prepared:

- Read through [Newcomers](newcomers)
- Make sure that you have built GHC once yourself.
- Your changes need to be validated. So make sure you validated GHC once. I suggest to have a second working copy of GHC that you only use to validate. There is a [section](working-conventions/git#workflow-with-validate) explaining how to do this.
- Fork [ghc on github](https://github.com/ghc/ghc/) (or otherwise publish a fork of the GHC repo) for easier collaboration during the hackathon.
- Get an account on this trac.
- Join `#ghc` on freenode.
- (optional, if you plan to stick around) Subscribe to `ghc-dev` and `ghc-tickets` mailing lists.

## Optional tips


If you have a strong remote machine with lots of cores, you can have the validate tree remotely.


For more convenient validation, especially if the validate repository is remotely, I (Joachim) have a script `ci-validate.sh` that waits for a new branch calls `validate/foo`, then validates it cleanly and either moves it to `validated/foo` or `broken/foo`. If you want to set up that as well, fetch the script from my [ghc-devscripts repository](https://github.com/nomeata/ghc-devscripts).

## Possible tickets



This is a list of tickets that might be suitable for a hacking sprint, but feel free to look for others (click “All Bugs“ and “All Tasks” on the left). And of course, feel free to extend this list.




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086, max: 0, desc: 1, order: id)
      </th>
<th>
        
        Summary (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086, max: 0, order: summary)
      </th>
<th>
        
        Owner (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086, max: 0, order: owner)
      </th>
<th>
        
        Type (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086, max: 0, order: type)
      </th>
<th>
        
        Status (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086, max: 0, order: status)
      </th>
<th>
        
        Priority (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086, max: 0, order: priority)
      </th>
<th>
        
        Milestone (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086, max: 0, order: milestone)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/17">#17</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/17">Separate warnings for unused local and top-level bindings</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/8.0.1">8.0.1</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/95">#95</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/95">GHCi :edit command should jump to the the last error</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      lortabac
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/%E2%8A%A5">⊥</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/393">#393</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/393">functions without implementations</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      Iceland_jack
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      new
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/%E2%8A%A5">⊥</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1262">#1262</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/1262">RecursiveDo in Template Haskell</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      mgsloan
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/%E2%8A%A5">⊥</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1388">#1388</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/1388">Newbie help features</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/%E2%8A%A5">⊥</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3314">#3314</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/3314">Add compilation date to +RTS --info</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ak3n
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4836">#4836</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/4836">literate markdown not handled correctly by unlit</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      new
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8429">#8429</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/8429">GHC.Base.{breakpoint, breakpointCond} do nothing</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      iand675
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      new
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8613">#8613</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/8613">simplifier ticks exhausted</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8959">#8959</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/8959">GHCi should honour UnicodeSyntax</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/8.0.1">8.0.1</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9086">#9086</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9086">main :: IO Int does different things with runghc and when compiled</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      gintas
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/7.10.1">7.10.1</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9095">#9095</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9095">make sdist picks up test files</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      thomie
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/8.2.1">8.2.1</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9122">#9122</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9122">Make Lint check for bad uses of `unsafeCoerce`</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      qnikst
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/8.0.1">8.0.1</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9127">#9127</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9127">Don&apos;t warn about pattern-bindings of the form `let !_ = rhs`</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9132">#9132</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9132">takeWhile&amp;C. still not fusible</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      skeuchel
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/7.10.1">7.10.1</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9136">#9136</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9136">Constant folding in Core could be better</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/8.6.1">8.6.1</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9156">#9156</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9156">Duplicate record field</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      gintas
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9177">#9177</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9177">Suggest Int when user uses int</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      nomeata
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      <a href="/trac/ghc/milestone/7.10.1">7.10.1</a>
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9178">#9178</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9178">improve orphan instance warning</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



## Summary


- *4* people particitpated: nomeata, gintas, skeuchel, Lorenzo *please add yourself*
- *8* Tickets worked on: [\#9177](https://gitlab.haskell.org/ghc/ghc/issues/9177), [\#8959](https://gitlab.haskell.org/ghc/ghc/issues/8959), [\#9127](https://gitlab.haskell.org/ghc/ghc/issues/9127), [\#9178](https://gitlab.haskell.org/ghc/ghc/issues/9178), [\#9132](https://gitlab.haskell.org/ghc/ghc/issues/9132), [\#393](https://gitlab.haskell.org/ghc/ghc/issues/393), [\#95](https://gitlab.haskell.org/ghc/ghc/issues/95), [\#9181](https://gitlab.haskell.org/ghc/ghc/issues/9181)
