{-# OPTIONS_GHC -fplugin=Plugin #-}

module Test where

import Test2
import TransAnn.Annotations

hello2 :: ([Annotation], Int)
hello2 = withAnnotations hello

