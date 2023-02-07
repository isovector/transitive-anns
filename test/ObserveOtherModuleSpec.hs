module ObserveOtherModuleSpec where

import qualified Data.Set as S
import Data.Set (Set)
import ObserveOtherModule
import Test.Hspec
import TransitiveAnns.Types

spec :: Spec
spec = describe "observe annotationsVal in imported module" $ do
  it "attached via ANN" $ observeAnn `shouldBe` S.fromList [Annotation Local "othermodule2" "ann"]
  it "attached via ANN'" $ observeAnn' `shouldBe` S.fromList [Annotation Local "othermodule2" "ann"]
  it "attached via AddAnnotation" $ observeAdd `shouldBe` S.fromList [Annotation Local "othermodule2" "add"]
  it "attached via AddAnnotation'" $ observeAdd' `shouldBe` S.fromList [Annotation Local "othermodule2" "add"]
