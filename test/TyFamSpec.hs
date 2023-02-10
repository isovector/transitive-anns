{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -ddump-tc-trace  #-}

module TyFamSpec where

import qualified Data.Set as S
import Data.Set (Set)
import TransitiveAnns.Types
import Test.Hspec
import GHC.TypeLits

data Component = Brig | Galley

type family ShowComponent (comp :: Component) where
  ShowComponent 'Brig = "brig"
  ShowComponent 'Galley = "galley"

test :: forall (comp :: Component) (name :: Symbol) x. AddAnnotation 'Remote (ShowComponent comp) name x => Int
test = 5

hello :: Int
hello = test @'Brig @"hello"


spec :: Spec
spec = do
  it "should work fine" $ do
    hello `shouldBe` 5
    -- annotated hello `shouldBe` S.fromList
    --   [ Annotation Remote "brig" "hello"
    --   ]

