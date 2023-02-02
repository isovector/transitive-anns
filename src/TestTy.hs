{-# LANGUAGE DeriveDataTypeable #-}
module TestTy where

import Data.Data (Data)

data Custom = Custom Int
  deriving (Eq, Ord, Show, Data)
