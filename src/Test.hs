{-# OPTIONS_GHC -fplugin=TransAnn.Plugin #-}

module Test where

import Test2
import TransAnn.Annotations

test :: ([Annotation], Int)
test = withAnnotations test2

