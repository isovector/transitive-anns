{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module Test2 where

import TransitiveAnns.Types
import Test3

{-# ANN test2 (track False) #-}
{-# ANN test2 (track "hello") #-}
test2 :: Int
test2 = test3

