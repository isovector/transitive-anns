{-# OPTIONS_GHC -fplugin=Plugin #-}

module Test3 where

import Ann
import Test

{-# ANN yo (Ann "soup" "b") #-}
yo :: Int
yo = hello2
