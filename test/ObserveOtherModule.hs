module ObserveOtherModule where

import ObserveOtherModule2
import TransitiveAnns.Types
import Data.Set (Set)

observeAnn :: Set Annotation
observeAnn = annotated otherRefAnn

observeAnn' :: Set Annotation
observeAnn' = annotated otherRefAnn'

observeAdd :: Set Annotation
observeAdd = annotated otherRefAdd

observeAdd' :: Set Annotation
observeAdd' = annotated otherRefAdd'

