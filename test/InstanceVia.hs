{-# LANGUAGE MultiParamTypeClasses #-}

module InstanceVia where

import qualified Data.Set as S
import TransitiveAnns.Types
import Test.Hspec
import GHC.TypeLits


testVia :: forall (comp :: Symbol) (name :: Symbol) x. AddAnnotation 'Remote comp name x => Int
testVia = 5

class HelloVia where
  helloVia :: Int

instance HelloVia where
  helloVia = testVia @"brig" @"helloVia"

