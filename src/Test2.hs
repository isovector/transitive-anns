{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module Test2 where

import TransitiveAnns.Types
import Test3

{-# ANN test2 (Annotation Remote "hello from" "test2") #-}
test2 :: Int
test2 = test3

