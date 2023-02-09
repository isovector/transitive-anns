module ObserveSameModule where

import Data.Set (Set)
import TransitiveAnns.Types

{-# ANN ref (Annotation Local "ref" "ref") #-}
ref :: Int
ref = 0

observeRef :: Set Annotation
observeRef = annotated ref


aref :: AddAnnotation 'Local "aref" "aref" x => Int
aref = 1

observeAref :: Set Annotation
observeAref = annotated aref

aref2 :: Int
aref2 = aref

observeAref2 :: Set Annotation
observeAref2 = annotated aref2
