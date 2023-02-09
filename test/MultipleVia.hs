module MultipleVia where

import TransitiveAnns.Types


{-# ANN vt1 (Annotation Local "vt1" "a") #-}
vt1 :: Bool
vt1 = False

{-# ANN vt2 (Annotation Local "vt2" "b") #-}
vt2 :: Bool
vt2 = False

{-# ANN vt3 (Annotation Remote "vt3" "c") #-}
{-# ANN vt3 (Annotation Remote "vt3" "d") #-}
vt3 :: Bool
vt3 = False

vt123 :: Bool
vt123 = and [vt1, vt2, vt3]

