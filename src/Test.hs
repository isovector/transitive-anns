{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fplugin=Plugin      #-}

module Test where

import Test2
import Data.Kind
import KnownAnns
import Ann

{-# ANN hello2 (Ann "hello" "b") #-}
{-# ANN hello2 (Ann "another" "b") #-}
hello2 :: [Ann]
hello2 = anns

