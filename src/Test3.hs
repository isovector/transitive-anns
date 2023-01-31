{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module Test3 where

import TransitiveAnns.Types

{-# ANN test3 (Annotation Remote "hello from" "test3") #-}
test3 :: Int
test3 = 4


