# Kirsten's to-do list

- Someday, download a full repo so that I can check that the patches I want to send can be applied to the HEAD. grrrrrrrr

## Demand analysis

1. Get code ready to check in:

  1. cleanup

    - files to go through:

      - basicTypes/Id.lhs
      - basicTypes/IdInfo.lhs
      - basicTypes/NewDemand.lhs
      - coreSyn/CoreLint.lhs
      - coreSyn/PprCore.lhs
      - iface/BinIface.lhs  
      - iface/IfaceSyn.lhs
      - iface/LoadIface.lhs
      - simplCore/SimplEnv.lhs
      - simplCore/SimplUtils.lhs
      - simplCore/Simplify.lhs
      - stranal/DmdAnal.lhs
      - stranal/WorkWrap.lhs
      - stranal/WwLib.lhs
      - types/Coercion.lhs
      - types/Type.lhs
  1. better comments (i.e., examples for things that may not work right)
  1. testing

1. Update Commentary 

1. Experiments:

  1. comparison with old strictness analyzer
  1. see whether new optimizations are helping
  1. remove special cases for `build` and see what happens

1. Paper

1. Modify worker/wrapper split

  1. so as to exploit the new analysis information

1. Think about the right way to handle this coercion stuff

1. Formal semantics?

# Old

[KirstenSandbox/GhcOldMac](kirsten-sandbox/ghc-old-mac) - building GHC on Mac OS 10.2.1 (the summary: don't do it.)
