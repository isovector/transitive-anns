{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications #-}

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

class AddAnnotation (loc :: Location) (api :: Symbol) (method :: Symbol) a

class HasAnnotation (loc :: Location) (api :: Symbol) (method :: Symbol)

class ToHasAnnotations a


class KnownAnnotations a where
  rawAnnotationsVal :: [Annotation]

annotationsVal :: forall x. KnownAnnotations x => Set Annotation
annotationsVal = S.fromList (rawAnnotationsVal @x)
{-# INLINE annotationsVal #-}


withAnnotations :: forall a x. KnownAnnotations x => a -> (Set Annotation, a)
withAnnotations a = (annotationsVal @x, a)
{-# INLINE withAnnotations #-}

annotated :: forall a x. KnownAnnotations x => a -> Set Annotation
annotated = fst . withAnnotations @a @x

