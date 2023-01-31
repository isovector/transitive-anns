{-# LANGUAGE MultiParamTypeClasses #-}
module KnownAnns where

import Ann


class KnownAnns where
  anns :: [Ann]

