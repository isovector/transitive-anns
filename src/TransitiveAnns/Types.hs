{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module TransitiveAnns.Types where

import qualified Data.Set as S
import Data.Set (Set)
import Data.Data (Typeable, Data)
import GHC.TypeLits (Symbol)

data Location = Local | Remote
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

data Annotation = Annotation
  { ann_location :: Location
  , ann_api      :: String
  , ann_method   :: String
  }
  deriving (Eq, Ord, Show, Typeable, Data)

class AddAnnotation (loc :: Location) (api :: Symbol) (method :: Symbol)

class KnownAnnotations where
  rawAnnotationsVal :: [Annotation]

annotationsVal :: KnownAnnotations => Set Annotation
annotationsVal = S.fromList rawAnnotationsVal
{-# NOINLINE annotationsVal #-}


withAnnotations :: KnownAnnotations => a -> (Set Annotation, a)
withAnnotations a = (annotationsVal, a)
{-# NOINLINE withAnnotations #-}

