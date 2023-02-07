{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module RingSpec where

import qualified Data.Set as S
import Data.Set (Set)
import TransitiveAnns.Types
import Ring
import Test.Hspec



r1a, r2a, r3a, r4a, r5a, r6a :: Set Annotation
r1a = annotated r1
r2a = annotated r2
r3a = annotated r3
r4a = annotated r4
r5a = annotated r5
r6a = annotated r6

aa1a, aa2a, aa3a, aa4a, aa5a, aa6a :: Set Annotation
aa1a = annotated aa1
aa2a = annotated aa2
aa3a = annotated aa3
aa4a = annotated aa4
aa5a = annotated aa5
aa6a = annotated aa6

spec :: Spec
spec =
  describe "check that annotations are transitive intra-module" $ do
    let ann = S.fromList [Annotation Local "ring" "6"]
    it "r1" $ r1a `shouldBe` ann
    it "r2" $ r2a `shouldBe` ann
    it "r3" $ r3a `shouldBe` ann
    it "r4" $ r4a `shouldBe` ann
    it "r5" $ r5a `shouldBe` ann
    it "r6" $ r6a `shouldBe` ann

    let aann = S.fromList [Annotation Local "cring" "6"]
    it "aa1" $ aa1a `shouldBe` aann
    it "aa2" $ aa2a `shouldBe` aann
    it "aa3" $ aa3a `shouldBe` aann
    it "aa4" $ aa4a `shouldBe` aann
    it "aa5" $ aa5a `shouldBe` aann
    it "aa6" $ aa6a `shouldBe` aann

