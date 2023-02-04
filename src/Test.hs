{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module Test where

import TransitiveAnns.Types
import Test2

test :: ([Annotation], Int)
test = withAnnotations test2

