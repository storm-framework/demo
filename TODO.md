# TODO 

## Plan

- [] Report source of missing symbol -- WHY are we resolving `EntityFieldWrapper`? 
	- What "module" ? 
	- What "thing"  ? 

- [] Build some sort of dependency graph to slice etc.

## Problem

1. import chain: `Config` -> `Storm.SMTP` -> `Storm.Core`
2. `Storm.Core` *defines* `EntityFieldWrapper` but 
3. `Storm.Core` *not imported* into the GHC env so name resolution fails

The bug disappears if you add 

```
import Storm.Core
```

to `src/Config.hs`



## Reproduce

```sh
$ LIQUID_DEV_MODE=true cabal v2-build
```

which after some grinding will produce

```
Branch to fix LH #1773

ghc: panic! (the 'impossible' happened)
  (GHC version 8.10.2:
	error-strictResolveSymUnknown type constructor `EntityFieldWrapper`
    ofBDataDecl-2
CallStack (from HasCallStack):
  error, called at src/Language/Fixpoint/Misc.hs:152:14 in liquid-fixpoint-0.8.10.2.1-inplace:Language.Fixpoint.Misc
```


## Fix: SpecDependencyGraph

```haskell
module SpecDep where 

data Label 
  = Sign 
  | Mesr 
  | Refl 
  | DCon 
  | TCon

data Node = MkNode 
  { nodeName  :: LocSymbol
  , nodeLabel :: Label
  }

type Graph = Map Node [Node]
```
