module TransitiveAnns.Plugin.Core where

import           GHC.Core.Class (classTyCon)
import           Data.String (fromString)
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import           TransitiveAnns.Plugin.Utils
import qualified TransitiveAnns.Types as TA


mkString :: String -> Expr Var
mkString str = mkListExpr charTy $ fmap mkCharExpr str


mkLoc :: TransitiveAnnsData -> TA.Location -> CoreExpr
mkLoc tad loc = mkConApp (tyConDataCons (tad_loc_tc tad) !! fromEnum loc) []



buildAnn :: TransitiveAnnsData -> TA.Annotation -> CoreExpr
buildAnn tad (TA.Annotation loc s str)
  = mkConApp (head $ tyConDataCons $ tad_ann_tc tad) $ [mkLoc tad loc, mkString s, mkString str]


mkAddAnnDict :: TransitiveAnnsData -> TA.Annotation -> CoreExpr
mkAddAnnDict tad (TA.Annotation loc api method) =
  mkConApp (head $ tyConDataCons $ classTyCon $ tad_add_ann tad)
    [ Type $ mkTyConTy $ promoteDataCon $ (!! fromEnum loc) $ tyConDataCons $ tad_loc_tc tad
    , Type $ mkStrLitTy $ fromString api
    , Type $ mkStrLitTy $ fromString method
    , Type $ anyTypeOfKind liftedTypeKind
    ]


mkKnownAnnsDict :: TransitiveAnnsData -> [TA.Annotation] -> CoreExpr
mkKnownAnnsDict tad z =
  mkConApp (head $ tyConDataCons $ classTyCon $ tad_knownanns tad)
    [Type (anyTypeOfKind liftedTypeKind) , buildCore tad z]


buildCore :: TransitiveAnnsData -> [TA.Annotation] -> Expr Var
buildCore tad anns = mkListExpr (mkTyConTy $ tad_ann_tc tad) $ fmap (buildAnn tad) anns


mkToHasAnnsDict :: TransitiveAnnsData ->  CoreExpr
mkToHasAnnsDict tad =
  mkConApp (head $ tyConDataCons $ classTyCon $ tad_to_has_ann tad)
    [Type (anyTypeOfKind liftedTypeKind)]
