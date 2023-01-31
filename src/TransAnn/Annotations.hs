{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TransAnn.Annotations where

import Data.Data (Typeable, Data)

data Annotation = Annotation
  { api :: String
  , call :: String
  }
  deriving (Eq, Ord, Show, Typeable, Data)


class KnownAnnotations where
  annotationsVal :: [Annotation]


withAnnotations :: KnownAnnotations => a -> ([Annotation], a)
withAnnotations a = (annotationsVal, a)

