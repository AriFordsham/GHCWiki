# GHC plans for 8.4.1


This page is our road-map for what will be in 8.4.  


If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Release in February 2018. Cut release branch in November 2017.

## Libraries Status


See [Libraries](status/ghc-8.4.1/libraries) and [Migration/8.4](migration/8.4).

## Release highlights (planned)


Below are the major highlights of 8.4.

- Improved support for **cross-compilation** (Moritz Angermann)

### Build system and miscellaneous changes

- Improved Windows support, including support for split sections and long file paths (Tamar Christina)
- Support for building stating libraries for elf and mach-o (`-staticlib`)

## Landed in `master` branch

- Improved code generation for join points
- Many, many bug-fixes

### Library changes

- Phase 2 of the [ Semigroup-Monoid Proposal](https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid) (Herbert Riedel)

### Build system and miscellaneous changes

- iserv can be used over the network via iserv-proxy
- llvm backend uses LLVM5
- **New Shake-based build system, `hadrian`, will be merged.**  (Andrey Mokhov)
- **Remove dependency on Hoopl package.**  (Michal Terepeta, [ Phab:D3616](https://phabricator.haskell.org/D3616))

## Tickets marked merge with no milestone




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)
      </th>
<th>
        
        Type (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td></tr>
<tr><td>
          </td>
<th>
            No tickets found
          </th>
<td>
        </td>
<td></td>
<td></td>
<td></td>
<td></td></tr></table>


  



## Tickets slated for 8.4.1


### merge/patch/upstream




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)
      </th>
<th>
        
        Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)
      </th>
<th>
        
        Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)
      </th>
<td>
    </td></tr>
<tr><td>
          </td>
<th>
            No tickets found
          </th>
<td>
        </td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td></tr></table>


  



### new




  
  
  
  
  
    

## Status: new (13 matches)


  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11196">#11196</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/11196">TypeInType performance regressions</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11198">#11198</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/11198">TypeInType error message regressions</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11371">#11371</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/11371">Bogus in-scope set in substitutions</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      niteria
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11523">#11523</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/11523">Infinite Loop when mixing UndecidableSuperClasses and the class/instance constraint synonym trick.</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      simonpj
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12088">#12088</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/12088">Type/data family instances in kind checking</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12506">#12506</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/12506">Compile time regression in GHC 8.</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      bgamari
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12564">#12564</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/12564">Type family in type pattern kind</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      goldfire
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13993">#13993</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/13993">Certain inter-module specializations run out of simplifier ticks</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14253">#14253</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14253">Pattern match checker mistakenly concludes pattern match on pattern synonym is unreachable</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14297">#14297</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14297">make bindist packages the wrong binaries for cross compilers</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      angerman
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14495">#14495</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14495">Relocatable GHC</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      angerman
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14856">#14856</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14856">GHC API: Linker failure on loading target multiple times</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14858">#14858</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14858">Typed hole subtitution search fails in the REPL</a>
                      
                      
                      
                      
                      
                      
                      
                      
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


  



### infoneeded




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td></tr>
<tr><td>
          </td>
<th>
            No tickets found
          </th>
<td>
        </td>
<td></td>
<td></td>
<td></td>
<td></td></tr></table>


  



