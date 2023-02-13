{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TransitiveAnns.Plugin.Utils where

import           Control.Monad (guard)
import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import           Data.Generics (everything, mkQ)
import           Data.Maybe (listToMaybe)
import qualified Data.Set as S
import           Data.String (fromString)
import           GHC (GhcTc, Class, SrcSpanAnn'(..))
import           GHC.Core.Class (classTyCon)
import           GHC.Data.Bag (bagToList)
import           GHC.Hs.Binds
import           GHC.Hs.Dump
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import           GHC.Tc.Plugin (findImportedModule, lookupOrig, tcLookupClass, tcLookupTyCon, FindResult(..))
import           GHC.Tc.Types.Constraint
import           GHC.Tc.Utils.Monad
import qualified TransitiveAnns.Types as TA


data TransitiveAnnsData = TransitiveAnnsData
  { tad_knownanns :: Class
  , tad_add_ann :: Class
  , tad_to_has_ann :: Class
  , tad_has_ann :: Class
  , tad_ann_tc :: TyCon
  , tad_loc_tc :: TyCon
  }


lookupTransitiveAnnsData :: TcPluginM TransitiveAnnsData
lookupTransitiveAnnsData = do
    Found _ tys_mod  <- findImportedModule (mkModuleName "TransitiveAnns.Types") Nothing
    known  <- lookupOrig tys_mod $ mkTcOcc "KnownAnnotations"
    add_ann  <- lookupOrig tys_mod $ mkTcOcc "AddAnnotation"
    to_has_ann  <- lookupOrig tys_mod $ mkTcOcc "ToHasAnnotations"
    has_ann  <- lookupOrig tys_mod $ mkTcOcc "HasAnnotation"
    ann  <- lookupOrig tys_mod $ mkTcOcc "Annotation"
    loc  <- lookupOrig tys_mod $ mkTcOcc "Location"

    TransitiveAnnsData
        <$> tcLookupClass known
        <*> tcLookupClass add_ann
        <*> tcLookupClass to_has_ann
        <*> tcLookupClass has_ann
        <*> tcLookupTyCon ann
        <*> tcLookupTyCon loc


pprTraceId :: Outputable a => String -> a -> a
pprTraceId x a = pprTrace x (ppr a) a

location :: Ct -> RealSrcSpan
location = ctLocSpan . ctLoc




parsePromotedAnn :: TransitiveAnnsData -> PredType -> Maybe TA.Annotation
parsePromotedAnn tad ty = do
  (tc, [loc_ty, api_ty, method_ty, _phantom]) <- splitTyConApp_maybe ty
  guard $ tc == classTyCon (tad_add_ann tad)

  (loc_tc, []) <- splitTyConApp_maybe loc_ty
  loc_dc <- isPromotedDataCon_maybe loc_tc
  guard $ dataConTyCon loc_dc == tad_loc_tc tad
  let loc = toEnum $ dataConTag loc_dc - 1

  api <- fmap unpackFS $ isStrLitTy api_ty
  method <- fmap unpackFS $ isStrLitTy method_ty
  pure $ TA.Annotation loc api method

locToDataCon :: TransitiveAnnsData -> TA.Location -> DataCon
locToDataCon tad loc = (!! fromEnum loc) $ tyConDataCons $ tad_loc_tc tad

toHasAnn :: TransitiveAnnsData -> TA.Annotation -> PredType
toHasAnn tad (TA.Annotation loc api method) =
  mkTyConApp (classTyCon (tad_has_ann tad))
    [ mkTyConTy $ promoteDataCon $ locToDataCon tad loc
    , mkStrLitTy $ fromString api
    , mkStrLitTy $ fromString method
    ]


getDec :: LHsBinds GhcTc -> RealSrcSpan -> Maybe (Id, HsBindLR GhcTc GhcTc)
getDec bs ss = listToMaybe $ do
  L _ b <- bagToList bs
  everything (<>) (mkQ [] $ \case
    L (SrcSpanAnn _ (RealSrcSpan ss' _)) (AbsBinds _ _ _ [(ABE _ poly mono _ _)] _ (bagToList -> [L _ (b@FunBind{})]) _)
      | containsSpan ss' ss
      -> pure (poly, b)
    L (SrcSpanAnn _ (RealSrcSpan ss' _)) (FunBind {fun_id = L _ n'})
      | containsSpan ss' ss
       -> pure (n', b)
    (L (SrcSpanAnn _ ss') x :: LHsBindLR GhcTc GhcTc) -> [] -- pprTrace "not looking at" (ppr (ss', showAstData BlankSrcSpan BlankEpAnnotations x)) []
    ) b

newtype A' = A' Annotation

instance Eq A' where
  A' (Annotation (NamedTarget na) (Serialized _ wos)) == A' (Annotation (NamedTarget na') (Serialized _ wos'))
    = na == na' && wos == wos'
  A' (Annotation NamedTarget{} _) == A' (Annotation ModuleTarget{} _)
    = False
  A' (Annotation ModuleTarget{} _) == A' (Annotation NamedTarget{} _)
    = False
  A' (Annotation (ModuleTarget m) (Serialized _ wos)) == A' (Annotation (ModuleTarget m') (Serialized _ wos'))
    = m == m' && wos == wos'

instance Ord A' where
  A' (Annotation (NamedTarget na) (Serialized _ wos)) `compare` A' (Annotation (NamedTarget na') (Serialized _ wos'))
    = compare na na' <> compare wos wos'
  A' (Annotation NamedTarget{} _) `compare` A' (Annotation ModuleTarget{} _)
    = LT
  A' (Annotation ModuleTarget{} _) `compare` A' (Annotation NamedTarget{} _)
    = GT
  A' (Annotation (ModuleTarget m) (Serialized _ wos)) `compare` A' (Annotation (ModuleTarget m') (Serialized _ wos'))
    = compare m m' <> compare wos wos'


addNoDups :: [Annotation] -> [Annotation] -> [Annotation]
addNoDups old new =
  let oldset = S.fromList (coerce @_ @[A'] old)
      newset = S.fromList (coerce @_ @[A'] new)
   in coerce (toList (oldset S.\\ newset)) ++ old
