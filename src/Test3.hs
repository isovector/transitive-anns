{-# OPTIONS_GHC -fplugin=TransAnn.Plugin #-}

module Test3 where

import TransAnn.Annotations

{-# ANN test3 (Annotation Remote "hello from" "test3") #-}
test3 :: Int
test3 = 4


