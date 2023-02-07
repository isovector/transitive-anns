module ObserveOtherModule2 where

import TransitiveAnns.Types

-- Has direct ANN
{-# ANN otherRefAnn (Annotation Local "othermodule2" "ann") #-}
otherRefAnn :: Int
otherRefAnn = 0

-- Has via transitivity
otherRefAnn' :: Int
otherRefAnn' = otherRefAnn

-- Has AddAnnotation; should attach to callsites
otherRefAdd :: AddAnnotation 'Local "othermodule2" "add" x => Int
otherRefAdd = 1

-- Solves the AddAnnotation, resulting in an ANN
otherRefAdd' :: Int
otherRefAdd' = otherRefAdd

