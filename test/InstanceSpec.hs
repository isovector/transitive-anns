{-# LANGUAGE MultiParamTypeClasses #-}

module InstanceSpec where

import qualified Data.Set as S
import Data.Set (Set)
import TransitiveAnns.Types
import Test.Hspec
import GHC.TypeLits
import InstanceVia


test :: forall (comp :: Symbol) (name :: Symbol) x. AddAnnotation 'Remote comp name x => Int
test = 5

class Hello where
  hello :: Int

instance Hello where
  hello = test @"brig" @"hello"

ahello :: Set Annotation
ahello = annotated hello

ahelloVia :: Set Annotation
ahelloVia = annotated helloVia

spec :: Spec
spec = do
  it "should propagate via instances" $ do
    ahello `shouldBe` S.fromList
      [ Annotation Remote "brig" "hello"
      ]

  it "should propagate via instances across modules" $ do
    ahelloVia `shouldBe` S.fromList
      [ Annotation Remote "brig" "helloVia"
      ]

