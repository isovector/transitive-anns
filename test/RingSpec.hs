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
spec = describe "check that annotations are transitive intra-module" $ do
  it "attached via ANN" $ do
    let ann = S.fromList [Annotation Local "ring" "1" , Annotation Local "ring" "6"]
    r1a `shouldBe` ann
    r2a `shouldBe` ann
    r3a `shouldBe` ann
    r4a `shouldBe` ann
    r5a `shouldBe` ann
    r6a `shouldBe` ann

  xit "attached via AddAnnotation" $ do
    -- TODO(sandy): There is a bug here; it fails to solve if there is a mutual
    -- loop in the AddAnnotation case
    let aann = S.fromList [{-Annotation Local "cring" "1", -}Annotation Local "cring" "6"]
    aa1a `shouldBe` aann
    aa2a `shouldBe` aann
    aa3a `shouldBe` aann
    aa4a `shouldBe` aann
    aa5a `shouldBe` aann
    aa6a `shouldBe` aann

