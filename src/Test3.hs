{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module Test3 where

import TransitiveAnns.Types
import TestTy

{-# ANN test3 (track True) #-}
{-# ANN test3 (track $ Custom 17) #-}
test3 :: Int
test3 = 4


