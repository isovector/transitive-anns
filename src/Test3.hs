{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}

{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module Test3 where

import TransitiveAnns.Types

test3 :: AddAnnotation 'Remote "hello from" "test3" x => Int
test3 = 4


