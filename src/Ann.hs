{-# LANGUAGE DeriveDataTypeable #-}

module Ann where

import Data.Data (Typeable, Data)

data Ann = Ann
  { api :: String
  , call :: String
  }
  deriving (Eq, Ord, Show, Typeable, Data)

