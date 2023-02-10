{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TransitiveAnns.Plugin.Utils where

import           GHC.Data.Bag (bagToList)
import           GHC.Core.Class (classTyCon)
import           GHC.Tc.Types.Constraint
import           Control.Monad (guard)
import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import           Data.Generics (everything, mkQ)
import           Data.Maybe (listToMaybe)
import qualified Data.Set as S
import           GHC (GhcTc, Class)
import           GHC.Hs.Binds
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import           GHC.Tc.Plugin (findImportedModule, lookupOrig, tcLookupClass, tcLookupTyCon, FindResult(..))
import           GHC.Tc.Utils.Monad
import qualified TransitiveAnns.Types as TA


data TransitiveAnnsData = TransitiveAnnsData
  { tad_knownanns :: Class
  , tad_add_ann :: Class
  , tad_ann_tc :: TyCon
  , tad_loc_tc :: TyCon
  }


lookupTransitiveAnnsData :: TcPluginM TransitiveAnnsData
lookupTransitiveAnnsData = do
    Found _ tys_mod  <- findImportedModule (mkModuleName "TransitiveAnns.Types") Nothing
    known  <- lookupOrig tys_mod $ mkTcOcc "KnownAnnotations"
    add_ann  <- lookupOrig tys_mod $ mkTcOcc "AddAnnotation"
    ann  <- lookupOrig tys_mod $ mkTcOcc "Annotation"
    loc  <- lookupOrig tys_mod $ mkTcOcc "Location"

    TransitiveAnnsData
        <$> tcLookupClass known
        <*> tcLookupClass add_ann
        <*> tcLookupTyCon ann
        <*> tcLookupTyCon loc


pprTraceId :: Outputable a => String -> a -> a
pprTraceId x a = pprTrace x (ppr a) a

location :: Ct -> Maybe Name
location ct = do
  h <- listToMaybe $ pprTraceId "location bndrs" $ tcl_bndrs $ ctl_env $ ctLoc ct
  Just $ case h of
    TcIdBndr var _ -> getName var
    TcIdBndr_ExpType na _ _ -> na
    TcTvBndr na _ -> na


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


getDec :: LHsBinds GhcTc -> Name -> Maybe (Id, HsBindLR GhcTc GhcTc)
getDec bs n = listToMaybe $ do
  L _ b <- bagToList bs
  everything (<>) (mkQ [] $ \case
    AbsBinds _ _ _ [(ABE _ poly mono _ _)] _ (bagToList -> [L _ (b@FunBind{})]) _
      | getName poly == n || getName mono == n
      -> pure (poly, b)
    -- FunBind {fun_id = L _ n'}
    --   | getOccName n == getOccName n' -> [(n', b)]
    --   | otherwise -> []
    (_ :: HsBindLR GhcTc GhcTc) -> []
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
