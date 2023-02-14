{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module TransitiveAnns.Plugin.Utils where

import           Control.Monad (guard)
import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import           Data.Generics (everything, mkQ, Data)
import           Data.Maybe (listToMaybe)
import           Data.Monoid (getLast)
import qualified Data.Set as S
import           Data.String (fromString)
import           GHC (GhcTc, Class, SrcSpanAnn'(SrcSpanAnn))
import           GHC.Core.Class (classTyCon)
import           GHC.Data.Bag (bagToList)
import           GHC.Hs.Binds
import           GHC.Hs.Expr
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import           GHC.Tc.Plugin (findImportedModule, lookupOrig, tcLookupClass, tcLookupTyCon, FindResult(..))
import           GHC.Tc.Types.Constraint
import           GHC.Tc.Utils.Monad
import qualified TransitiveAnns.Types as TA


------------------------------------------------------------------------------
-- | All the wired-in things we need to track.
data TransitiveAnnsData = TransitiveAnnsData
  { tad_knownanns  :: Class  -- ^ KnownAnnotations
  , tad_add_ann    :: Class  -- ^ AddAnnotation
  , tad_to_has_ann :: Class  -- ^ ToHasAnnotations
  , tad_has_ann    :: Class  -- ^ HasAnnotation
  , tad_ann_tc     :: TyCon  -- ^ Annotation
  , tad_loc_tc     :: TyCon  -- ^ Location
  }


------------------------------------------------------------------------------
-- | Build a 'TransitiveAnnsData' by looking up everything.
lookupTransitiveAnnsData :: TcPluginM TransitiveAnnsData
lookupTransitiveAnnsData = do
    Found _ tys_mod
      <- findImportedModule (mkModuleName "TransitiveAnns.Types") Nothing
    known      <- lookupOrig tys_mod $ mkTcOcc "KnownAnnotations"
    add_ann    <- lookupOrig tys_mod $ mkTcOcc "AddAnnotation"
    to_has_ann <- lookupOrig tys_mod $ mkTcOcc "ToHasAnnotations"
    has_ann    <- lookupOrig tys_mod $ mkTcOcc "HasAnnotation"
    ann        <- lookupOrig tys_mod $ mkTcOcc "Annotation"
    loc        <- lookupOrig tys_mod $ mkTcOcc "Location"

    TransitiveAnnsData
        <$> tcLookupClass known
        <*> tcLookupClass add_ann
        <*> tcLookupClass to_has_ann
        <*> tcLookupClass has_ann
        <*> tcLookupTyCon ann
        <*> tcLookupTyCon loc



------------------------------------------------------------------------------
-- | Helper function for debugging. Like traceShowId, but for internal GHC
-- things.
pprTraceId :: Outputable a => String -> a -> a
pprTraceId x a = pprTrace x (ppr a) a


------------------------------------------------------------------------------
-- | Get the real source span that caused a constraint to be emitted.
location :: Ct -> RealSrcSpan
location = ctLocSpan . ctLoc


------------------------------------------------------------------------------
-- | Turn a @'Annotation loc api method@ (at the type level) into an
-- 'TA.Annotation' (at compile time.) This will fail if there are any stuck
-- type families or polytypes.
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


------------------------------------------------------------------------------
-- | Get the data constructor corresponding to an enum value.
enumToDataCon :: Enum a => TyCon -> a -> DataCon
enumToDataCon tc a = tyConDataCons tc !! fromEnum a


------------------------------------------------------------------------------
-- | Get the datacon for a 'Ta.Location'.
locToDataCon :: TransitiveAnnsData -> TA.Location -> DataCon
locToDataCon = enumToDataCon . tad_loc_tc


------------------------------------------------------------------------------
-- | Transform a 'TA.Annotation' into a corresponding 'HasAnnotation'
-- constraint.
toHasAnn :: TransitiveAnnsData -> TA.Annotation -> PredType
toHasAnn tad (TA.Annotation loc api method) =
  mkTyConApp (classTyCon (tad_has_ann tad))
    [ mkTyConTy $ promoteDataCon $ locToDataCon tad loc
    , mkStrLitTy $ fromString api
    , mkStrLitTy $ fromString method
    ]


------------------------------------------------------------------------------
-- | Given top-level bindings, determine which one contains the given
-- 'RealSrcSpan'.
getDec :: LHsBinds GhcTc -> RealSrcSpan -> Maybe (Id, HsBindLR GhcTc GhcTc)
getDec bs ss = listToMaybe $ do
  L _ b <- bagToList bs
  everything (<>) (mkQ [] $ \case
      L (SrcSpanAnn _ (RealSrcSpan ss' _))
        (AbsBinds _ _ _ [ABE _ poly _ _ _] _ (bagToList -> [L _ FunBind{}]) _)
        | containsSpan ss' ss -> pure (poly, b)
      L (SrcSpanAnn _ (RealSrcSpan ss' _)) (FunBind {fun_id = L _ n'})
        | containsSpan ss' ss -> pure (n', b)
      (_ :: LHsBindLR GhcTc GhcTc) -> []
    ) b


------------------------------------------------------------------------------
-- | Given an RSS, find the nearest application node that applies it. For
-- example,
--
-- @@
--  foo (bar qux)
--       ^^^
-- @@
--
-- will return @bar qux@.
getCallingExpr :: Data a => RealSrcSpan -> a -> Maybe (LHsExpr GhcTc)
getCallingExpr ss a = getLast $ everything (<>) (mkQ mempty
  $ \case
      HsApp _ (L (SrcSpanAnn _ (RealSrcSpan ss' _)) _) b
        | ss' == ss -> pure b
      OpApp _ (L (SrcSpanAnn _ (RealSrcSpan ss' _)) _) _ b
        | ss' == ss -> pure b
      (_ :: HsExpr GhcTc) -> mempty
  ) a


------------------------------------------------------------------------------
-- | Stupid newtype to give an 'Ord' instance to 'Annotation'.
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


------------------------------------------------------------------------------
-- | Add new annotations to a list, without adding any duplicates, but not
-- removing duplicates which might already exist.
addNoDups :: [Annotation] -> [Annotation] -> [Annotation]
addNoDups old new =
  let oldset = S.fromList (coerce @_ @[A'] old)
      newset = S.fromList (coerce @_ @[A'] new)
   in coerce (toList (oldset S.\\ newset)) ++ old

