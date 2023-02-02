{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module Test where

import TransitiveAnns.Types
import Test2
import TestTy

test :: ([Custom], Int)
test = withAnnotations test2

