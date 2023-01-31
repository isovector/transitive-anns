# transitive-anns

## Overview

This package is a small compiler plugin that propagates, and reifies, annotations
transitively. To get started, try attaching some annotations to some values:

```haskell
{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

import TransitiveAnns.Types

{-# ANN test3 (Annotation Remote "hello from" "test3") #-}
test3 :: Int
test3 = 4

{-# ANN test2 (Annotation Remote "hello from" "test2") #-}
test2 :: Int
test2 = test3
```

and then we can reify them:

```haskell
test :: ([Annotation], Int)
test = withAnnotations test2
```

with result:

```haskell
( [ Annotation Remote "hello from" "test3"
  , Annotation Remote "hello from" "test2"
  ]
, 4
)
```

Better yet, it works across module boundaries!


## Nitty Gritty Details

Internally, this plugin consists of two parts: a CoreToDo plugin, and a
typechecker plugin. The CoreToDo runs after the module has been compiled, looks
up every function with an `Annotation`, and then also attaches it to any
function which *calls* an annotated function. These get stuck in the `ModIface`,
so they are cached in the compiled artifacts.

The other half of the plugin is a solver for the following class:

```haskell
class KnownAnnotations where
  annotationsVal :: [Annotation]
```

which returns every annotation attached to the *function calling*
`annotationsVal`. Of course, this dispatch plays nicely with the usual
constraint system, so you can delay its evaluation by adding a
`KnownAnnotations =>` given constraint to your type signature.

