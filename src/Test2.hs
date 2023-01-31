{-# OPTIONS_GHC -fplugin=Plugin #-}

module Test2 where

import TransAnn.Annotations

{-# ANN hello (Annotation Remote "hello" "world") #-}
hello :: Int
hello = 5

another :: Int
another = hello
