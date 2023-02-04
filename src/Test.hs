{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module Test where

import Data.Set (Set)
import TransitiveAnns.Types
import Test2

test :: (Set Annotation, Int)
test = withAnnotations test2

