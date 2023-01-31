{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TransitiveAnns.Types where

import Data.Data (Typeable, Data)

data Location = Local | Remote
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

data Annotation = Annotation
  { ann_location :: Location
  , ann_api      :: String
  , ann_method   :: String
  }
  deriving (Eq, Ord, Show, Typeable, Data)


class KnownAnnotations where
  annotationsVal :: [Annotation]


withAnnotations :: KnownAnnotations => a -> ([Annotation], a)
withAnnotations a = (annotationsVal, a)

