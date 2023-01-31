{-# OPTIONS_GHC -fplugin=Plugin #-}

module Test2 where

import Ann

{-# ANN hello (Ann "hello" "world") #-}
hello :: Int
hello = 5

another :: Int
another = hello
