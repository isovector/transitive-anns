{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module LetBindingSpec where

import GHC.TypeLits
import qualified Data.Set as S
import Data.Set (Set)
import TransitiveAnns.Types
import Test.Hspec

fedClient
    :: forall (api :: Symbol) (name :: Symbol) x
     . AddAnnotation 'Remote api name x
    => Bool
    -> Int
fedClient _ = 5

notifyUserDeleted :: String -> ()
notifyUserDeleted str = do
  let b = null str
  const () $
    fedClient @"brig" @"on-user-deleted-connections" $
      b

spec :: Spec
spec = do
  it "should correctly expand complicated function body" $ do
    annotated notifyUserDeleted `shouldBe` S.fromList
      [ Annotation Remote "brig" "on-user-deleted-connections"
      ]

