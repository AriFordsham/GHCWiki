# Continuous Integration


This page is to support the discussion of GHC DevOps Group on the CI solution for GHC to provide continuous testing and release artefact generation. See also [\#13716](https://gitlab.haskell.org//ghc/ghc/issues/13716).


If you are looking for information about how to maintain our new CircleCI and Appveyor infrastructure see [ContinuousIntegration/Usage](continuous-integration/usage).

## Status

- CircleCI: [ https://circleci.com/gh/ghc/ghc](https://circleci.com/gh/ghc/ghc)
- Appveyor: [ https://ci.appveyor.com/project/GHCAppveyor/ghc](https://ci.appveyor.com/project/GHCAppveyor/ghc)
- Docker images for CircleCI deployment: [ https://hub.docker.com/r/ghcci/](https://hub.docker.com/r/ghcci/)

## Requirements



**Primary** 


- Build GHC for Tier 1 platforms (Windows, Linux & macOS), run `./validate`, and produce release artefacts (distributions and documentation).
- Build PRs (differentials) and run `./validate` on Linux/x86-64.
- Security: PRs builds run arbitrary user code; this must not be able to compromise other builds and especially not release artefacts.
- Infrastructure reproducibility (infrastructure can be spun up and configured automatically)
- Infrastructure forkability (devs forking the GHC repo can run their own CI without additional work)
- Low maintenance overhead
- Low set up costs

**Secondary**

- Build PRs (differentials) and run `./validate` on non-Linux/x86-64 Tier 1 platforms.
- Build GHC for non-Tier 1 platforms & run `./validate`

## Possible solutions

### Jenkins


Pros

- We can run build nodes on any architecture and OS we choose to set up.


Cons

- Security is low on PR builds unless we spend further effort to sandbox builds properly. Moreover, even with sandboxing, Jenkins security record is troublesome.
- Jenkins is well known to be time consuming to set up.
- Additional time spent setting up servers.
- Additional time spent maintaining servers.
- It is unclear how easy it is to make the set up reproducible.
- The set up is not forkable (a forker would need to set up their own servers).

### CircleCI & AppVeyor


Pros

- Easy to set up
- Excellent security
- Low maintenance cost
- Infrastructure reproducible 
- Infrastructure forkable
- Easy to integrate with GitHub


Cons

- Direct support only for Linux, Windows, macOS, iOS & Android, everything else needs to rely on cross-compiling and QEMU, or building on remote drones.
- We are limited to CircleCI's & Appveyor's current feature set and have to rely on them for feature development.
- We need to deal with two different CI providers.

## Discussion summary


After a detailed discussion on the GHC DevOps Group mailing list [ https://mail.haskell.org/pipermail/ghc-devops-group/](https://mail.haskell.org/pipermail/ghc-devops-group/), the group reached the conclusion that all factors considered, CircleCI & AppVeyor provides the best trade offs for the following reasons.

**Costs**

- Both solutions incur hardware or subscription costs. In the case of Jenkins, we need servers (and we just learnt that RackSpace terminated its OSS program at the end of the year) and, in the case of CircleCI & AppVeyor, we will likely need more resource than they provide for free to open source projects. However, Google X and Tweag I/O have indicated that they would be happy to help cover such costs.
- Both solutions incur some developer costs. However, by all accounts and our own experience, this is substantially lower for the CircleCI & AppVeyor solution. In particular, it completely eliminates server maintenance costs. Integration with GitHub is straight forward and Phabricator also seems to have some support.


Overall, CircleCI & AppVeyor are more cost-effective.

**Flexibility**

- Jenkins is fully customisable and provides complete flexibility wrt to the architectures and operations systems used in build machines.
- With CircleCI & AppVeyor, we are dependent on the features provided by the hosting services and limited in the architectures and OSes that are directly supported. In particular, we will have to fall back to cross-compiling and using QEMU for less commonly used platforms. Our base line, the Tier 1 platforms, as well as Android and iOS on ARM are supported by CircleCI & AppVeyor.


Overall, Jenkins is more flexible.

**Security**

- Security is crucial as we are planning to run untrusted and unreviewed code in PRs/differentials in the CI environment.
- To provide adequate security, we need to add sandboxing to Jenkins (which is possible, but more work). However, even with that, the security track record of Jenkins is not very good. 
- In the case of CircleCI & AppVeyor, security is taken care of by the provider.


Overall, CircleCI & AppVeyor are more secure.

**Scalability**

- With forkability, we refer to a user's ability to fork GHC, modify it, and run their own CI instance (not using GHC HQ's infrastructure) without any significant extra work. This significantly improves scaling and takes load of the GHC HQ's infrastructure. CircleCI & AppVeyor is forkable, but Jenkins is not (as a user needs to set up their own infrastructure).
- While Jenkins can be combined with other Docker/Kubernetes to improve scaling, this is difficult (e.g., Tweag I/O had some rather bad experiences with this). In contrast, this is handled by the provider in the CircleCI & AppVeyor case.


Overall, CircleCI & AppVeyor are more scalable.

**Reliability**

- With our own infrastructure, reliability is in our own hands, but we will not be able (as it would be too expensive) to provide any real reliability guarantees or support with guaranteed turn around times.
- In the case of CircleCI & AppVeyor we depend on the providers for a reliable service. Ben talked to the Rust team who were very positive about CircleCI in this respect, but at the end of the day, it will be hard to know without trying (given we want to rely mostly on free open-source plans).


Overall, let's call this a tie.

## Usage estimate


A quick back-of-the-envelope calculation suggests that to simply keep up
with our current average commit rate (around 200 commits/month) for the
four environments that we currently build we need a bare minimum of:

```wiki
   200 commit/month
 * 4 build/commit             (Linux/i386, Linux/amd64,
                               OS X, Windows/amd64)
 * 2.5 CPU-hour/build         (approx. average across platforms
                               for a validate)
 / (2 CPU-hour/machine-hour)  (CircleCI appears to use 2 vCPU instances)
 / (30*24 machine-hour/month)
 ~ 2 machines
```


Note that this doesn't guarantee reasonable wait times but rather merely
ensure that we can keep up on the mean. On top of this, we see around
300 differential revisions per month. This requires another 3 machines
to keep up.


So, we need at least five machines but, again, this is a minimum;
modelling response times is hard but I expect we would likely need to
add at least two more machines to keep response times in the
contributor-friendly range, especially considering that under Circle CI
we will lose the ability to prioritize jobs (by contrast, with Jenkins
we can prioritize pull requests as this is the response time that we
really care about). Now consider that we would like to add at least
three more platforms (FreeBSD, OpenBSD, Linux/aarch64, all of which may
be relatively slow to build due to virtualisation overhead) as well as a
few more build configurations on amd64 (LLVM, unregisterised, at least
one cross-compilation target) and a periodic slow validation and we may
be at over a dozen machines.

## Status


Circle CI & AppVeyor integration on the Tweag GHC fork: [ https://github.com/tweag/ghc/tree/tweag/ci](https://github.com/tweag/ghc/tree/tweag/ci)

- Linux/x86_64 (CircleCI): build & store artefacts works
- macOS/x86_64 (CircleCI): build & store artefacts works
- Windows/x86_64 (AppVeyor): asked for increased limits

**Update (Jul. 24th, 2018):** We wrote a small web application meant to act as a bridge between Phabricator and Circle CI. We have successfully used it to run Circle CI builds against Phabricator differentials. The code lives [ here](https://github.com/alpmestan/phab-circleci-bridge). We will soon be deploying it and trying to use it for several job types. See [ here](https://phabricator.haskell.org/harbormaster/build/49274/) for an example of using this setup.

## Todo


Below, we track all work that needs to be done until we have achieved the following specific goals:

- automatic per-commit builds of master for Linux/x86_64 on CircleCI (per each block of commits from one PR/Differential would be sufficient),
- automatic builds of Linux/i386, macOS/x86_64 & Windows/x86_64 on CircleCI and AppVeyor at least once per day,
- automatic Linux/x86_64 build for each PR/Differential on CircleCl,
- automatic generation of all release artefacts on Linux/i386, Linux/x86_64, macOS/x86_64 & Windows/x86_64 on CircleCI and AppVeyor at least once per day,
- end-to-end testing by creating a source distribution, from that a binary distribution, and use that for regression testing. (This has the advantage of also testing the distribution creation process.)

### Relevant Trac tickets



The following are issues about the CI system itself,




  
  
  
  
  
    

## Status: new (18 matches)


  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: new, component: Continuous+Integration, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)
      </th>
<th>
        
        Type (Ticket query: status: new, component: Continuous+Integration, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: new, component: Continuous+Integration, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: new, component: Continuous+Integration, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: new, component: Continuous+Integration, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11958">#11958</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/11958">Improved testing of cross-compiler</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13122">#13122</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/13122">Investigate reporting build errors with harbormaster.sendmessage</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14416">#14416</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14416">CI with CircleCI</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14475">#14475</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14475">Upload documentation dumps</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14502">#14502</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14502">Build Alpine Linux binary distributions</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14505">#14505</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14505">CircleCI only builds pushed heads</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14508">#14508</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14508">Bring up Appveyor for Windows CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      mrkkrp
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14599">#14599</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14599">32-bit Windows test environment</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14949">#14949</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14949">Perform builds on non-Debian-based systems on Circle CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      mrkkrp
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15011">#15011</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15011">Automate update of VersionHistory table</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15582">#15582</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15582">Phabricator shows &quot;drafts&quot; by default</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15699">#15699</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15699">Run sanity checker in more testsuite runs</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15749">#15749</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15749">Long Harbormaster builds</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15972">#15972</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15972">Test nofib tests for correctness in CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16134">#16134</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16134">Continuous integration on FreeBSD</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16200">#16200</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16200">Mechanical checking of submodule versions for releases</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16264">#16264</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16264">Install reqlib&apos;d libraries during CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16355">#16355</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16355">Save CI performance metrics on windows jobs</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      davide
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  




The following are GHC issues which are currently breaking CI builds,




  
  
  
  
  
    

## Status: new (11 matches)


  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: new, keywords: ~ci-breakage, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)
      </th>
<th>
        
        Type (Ticket query: status: new, keywords: ~ci-breakage, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: new, keywords: ~ci-breakage, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: new, keywords: ~ci-breakage, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: new, keywords: ~ci-breakage, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14823">#14823</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/14823">Test profiling/should_run/scc001 fails on Circle CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15059">#15059</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15059">ghcpkg05 fails</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15064">#15064</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15064">T8089 mysteriously fails when GHC is built with LLVM</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      osa1
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15072">#15072</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15072">Teach the testsuite driver about response files</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ckoparkar
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15382">#15382</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/15382">heapprof001 segfaults in prof_hc_hb way on i386</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16085">#16085</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16085">ffi018_ghci fails when unregisterised</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16100">#16100</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16100">T11627a fails sometimes on CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16193">#16193</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16193">Nondeterministic T15897 timeout failures</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16349">#16349</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16349">Nondeterministic T3424 timeouts on CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16350">#16350</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16350">Nondeterministic T5559 failures on CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
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
<th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16361">#16361</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org//ghc/ghc/issues/16361">Non-deterministic hs_try_putmvar00 failure on CI</a>
                      
                      
                      
                      
                      
                      
                      
                      
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


  



### General


- Talk to CircleCI about increased limits for the free plan for GHC. Determine how much on top of that we need.
- AppVeyor has a 1h limit for OSS projects by default. Determine how much on top of the OSS plan we need and whether they are willing to relax the limits for a visible OSS project. **Status:** wrote email, but no response yet.

### Per-commit build on Linux/x86_64



Probably easiest to just trigger those builds from GitHub (as all commits are mirrored there anyway).


- Need to get builds on every individual commit (e.g., to do easy bisection) — see [\#14505](https://gitlab.haskell.org//ghc/ghc/issues/14505). **Alternative:** Use GitHub PRs instead of pushing to master directly.

### Daily builds on Linux/i386, macOS/x86_64 & Windows/x86_64


- Implement AppVeyor build config. **Blocked:** waiting for AppVeyor to increase time limit. (Apparently, Rust are on a payed plan, so maybe we have to do that, too.)
- Linux/i386 ought to be a small change on Linux/x86_64, or is there more to it?
- *Low priority:* Implement end-to-end testing. **Blocked:** on [\#14392](https://gitlab.haskell.org//ghc/ghc/issues/14392), [\#14411](https://gitlab.haskell.org//ghc/ghc/issues/14411) & [\#14412](https://gitlab.haskell.org//ghc/ghc/issues/14412).
- *Low priority:* We want to run `./validate --slow` at some point — see [\#13205](https://gitlab.haskell.org//ghc/ghc/issues/13205).
- *Low priority:* We also want to run LLVM and unregisterised builds

### Per-PR/Differential build on Linux/x86_64

- This is the CircleCI 1.0 documentation on Phabricator integration: [ https://circleci.com/docs/1.0/phabricator/](https://circleci.com/docs/1.0/phabricator/) Does this work with CircleCI 2.0 as well? (The CircleCI API 1.1 supposedly can drive both.)
- Implement CircleCI/GitHub integration for PRs.

### Daily release artifacts for all Tier 1 platforms


- Tar balls are currently being put into CircleCI artefacts store (where they will be kept for one month).
- *Low priority:* Implement S3 upload for longer term storage.
- Documentation bundle for the website needs to be generated and uploaded. Might be easier with Hadrian: [ https://github.com/snowleopard/hadrian/pull/413](https://github.com/snowleopard/hadrian/pull/413)
