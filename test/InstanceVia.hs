{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module InstanceVia where

import TransitiveAnns.Types
import GHC.TypeLits


testVia :: forall (comp :: Symbol) (name :: Symbol) x. AddAnnotation 'Remote comp name x => Int
testVia = 5

class HelloVia where
  helloVia :: Int

instance HelloVia where
  helloVia = testVia @"brig" @"helloVia"

