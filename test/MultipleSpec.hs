module MultipleSpec where

import qualified Data.Set as S
import Data.Set (Set)
import TransitiveAnns.Types
import Test.Hspec
import MultipleVia


{-# ANN t1 (Annotation Local "t1" "a") #-}
t1 :: Bool
t1 = False

{-# ANN t2 (Annotation Local "t2" "b") #-}
t2 :: Bool
t2 = False

{-# ANN t3 (Annotation Remote "t3" "c") #-}
{-# ANN t3 (Annotation Remote "t3" "d") #-}
t3 :: Bool
t3 = False

t123 :: Bool
t123 = and [t1, t2, t3]

obs :: Set Annotation
obs = annotated t123

vobs :: Set Annotation
vobs = annotated vt123


spec :: Spec
spec = do
  it "should collect annotations locally" $ do
    obs `shouldBe` S.fromList
      [ Annotation Local "t1" "a"
      , Annotation Local "t2" "b"
      , Annotation Remote "t3" "c"
      , Annotation Remote "t3" "d"
      ]

  it "should collect annotations when importing" $ do
    vobs `shouldBe` S.fromList
      [ Annotation Local "vt1" "a"
      , Annotation Local "vt2" "b"
      , Annotation Remote "vt3" "c"
      , Annotation Remote "vt3" "d"
      ]

