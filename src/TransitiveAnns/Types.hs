{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TransitiveAnns.Types where

import Data.Data (Typeable, Data, typeRepFingerprint, typeRep)
import Data.Maybe (mapMaybe)
import GHC.Fingerprint.Type (Fingerprint (Fingerprint))
import Data.Word (Word64)
import Data.Proxy (Proxy(Proxy))
import GhcPlugins (serializeWithData, deserializeWithData)

data TrackAnn = TrackAnn
  { ta_fingerprint_w1 :: Word64
  , ta_fingerprint_w2 :: Word64
  , ta_data :: [Word64]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

fingerprint :: forall a. Data a => Fingerprint
fingerprint = typeRepFingerprint $ typeRep $ Proxy @a

track :: forall a. Data a => a -> TrackAnn
track = TrackAnn w1 w2 . fmap fromIntegral . serializeWithData
  where
    Fingerprint w1 w2 = fingerprint @a

class KnownAnnotations where
  rawAnnotationsVal :: [TrackAnn]

annotationsVal :: (KnownAnnotations, Data a) => [a]
annotationsVal = mapMaybe fromTrackAnn rawAnnotationsVal

fromTrackAnn :: forall a. Data a => TrackAnn -> Maybe a
fromTrackAnn (TrackAnn w1 w2 d)
  | Fingerprint w1 w2 == fingerprint @a =
      Just $ deserializeWithData $ fmap fromIntegral d
  | otherwise = Nothing

withAnnotations :: (KnownAnnotations, Data a) => b -> ([a], b)
withAnnotations b = (annotationsVal, b)

