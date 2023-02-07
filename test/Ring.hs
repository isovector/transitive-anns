{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ring where

import TransitiveAnns.Types

-- Test ANN-otated rings
r1, r2, r3, r4, r5, r6 :: Int
r1 = r6
r2 = r1
r3 = r2
r4 = r3
r5 = r4

{-# ANN r6 (Annotation Local "ring" "6") #-}
r6 = r5

-- Test AddAnnotation rings
aa1, aa2, aa3, aa4, aa5 :: Int
aa1 = aa6
aa2 = aa1
aa3 = aa2
aa4 = aa3
aa5 = aa4

aa6 :: AddAnnotation 'Local "cring" "6" x => Int
aa6 = aa5

