{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module TransitiveAnns.Plugin where

import           Bag (bagToList)
import           Class (classTyCon)
import           Constraint
import           Data.Coerce (coerce)
import           Data.Data hiding (TyCon)
import           Data.Foldable (fold, toList)
import           Data.Functor ((<&>))
import           Data.Generics (everything, mkQ)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC (GhcTc, Class)
import           GHC.Hs.Binds
import           GhcPlugins hiding (TcPlugin, (<>), empty)
import           TcEvidence (EvTerm(EvExpr))
import           TcPluginM (findImportedModule, lookupOrig, tcLookupClass, tcLookupTyCon)
import           TcRnMonad
import qualified TransitiveAnns.Types as TA
import Control.Monad (guard)

------------------------------------------------------------------------------

forBinds :: Ord b => (Expr b -> r) -> Bind b -> Map b r
forBinds f (NonRec b ex) = M.singleton b $ f ex
forBinds f (Rec x0) = foldMap (\(b, e) -> M.singleton b $ f e) x0

lookupAttachedAnns :: AnnEnv -> Set CoreBndr -> Map CoreBndr [TA.Annotation]
lookupAttachedAnns annenv = foldMap $ \b ->
  M.singleton b $ findAnns (deserializeWithData @TA.Annotation) annenv $ NamedTarget $ getName b

referencedVars :: [Bind CoreBndr] -> Map CoreBndr (Set CoreBndr)
referencedVars bs = flip foldMap bs $ forBinds getVars

getVars :: Data a => a -> Set Var
getVars =
  everything (<>) $ mkQ mempty S.singleton

withAnnotations :: Ord b => Map b [c] -> Map a (Set b) -> Map a [c]
withAnnotations anns cs = M.map (foldMap (fromMaybe [] . flip M.lookup anns)) cs

buildNewAnnotations :: Map CoreBndr [TA.Annotation] -> [Annotation]
buildNewAnnotations annotated = do
  (b, as) <- M.toList annotated
  as <&> \a -> Annotation (NamedTarget $ getName b) $ toSerialized serializeWithData a


data TransitiveAnnsData = TransitiveAnnsData
  { tad_knownanns :: Class
  , tad_add_ann :: Class
  , tad_ann_tc :: TyCon
  , tad_loc_tc :: TyCon
  }


------------------------------------------------------------------------------
-- | Lookup the classes from 'Data.Constraint.Emerge' and build an
-- 'EmergeData'.
lookupTransitiveAnnssData :: TcPluginM TransitiveAnnsData
lookupTransitiveAnnssData = do
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


transann :: ModGuts -> CoreM ModGuts
transann mg = do
  hsc <- getHscEnv
  annenv <- liftIO $ prepareAnnotations hsc $ Just mg
  let mganns = mg_anns mg
  let contents = referencedVars $ mg_binds mg
      all_ids = fold contents
      anns = lookupAttachedAnns annenv all_ids
      annotated = withAnnotations anns contents
      new_anns = buildNewAnnotations annotated
  pure $ mg { mg_anns = mganns <> new_anns }

location :: TcBinderStack -> Maybe Name
location tcbs = do
  h <- listToMaybe tcbs
  Just $ case h of
    TcIdBndr var _ -> getName var
    TcIdBndr_ExpType na _ _ -> na
    TcTvBndr na _ -> na

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = const $ pure . (CoreDoPluginPass "TransitiveAnns" transann :)
  , tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = lookupTransitiveAnnssData
      , tcPluginSolve = solve

      , tcPluginStop = const $ pure ()
      }
  , pluginRecompile  = purePlugin
  }

findWanted :: Class -> Ct -> Maybe Ct
findWanted c ct = do
  let p = ctev_pred $ cc_ev ct
  case splitTyConApp_maybe p of
    Just (x, _) ->
      case x == classTyCon c of
        True -> Just ct
        False -> Nothing
    _ -> Nothing

solve :: TransitiveAnnsData -> TcPluginSolver
solve tad _ _ ws = do
  let over k f = traverse (k tad) $ mapMaybe (findWanted $ f tad) ws
  pprTraceM "all wanteds" $ ppr $ fmap (splitTyConApp_maybe . ctev_pred . cc_ev) ws
  pprTraceM "all wanted spans" $ ppr $ fmap (tcl_bndrs . ctl_env . ctLoc) ws

  adds   <- over solveAddAnn    tad_add_ann
  knowns <- over solveKnownAnns tad_knownanns
  let res = concat $ adds <> knowns
  pure $ TcPluginOk res []


solveKnownAnns :: TransitiveAnnsData -> Ct -> TcPluginM [(EvTerm, Ct)]
solveKnownAnns tad known = do
  -- span <- unsafeTcPluginTcM $ fmap tcl_loc getLclEnv
  -- pprTraceM "span" $ ppr $ tcl_bndrs $ ctl_env $ ctLoc known
  env <- unsafeTcPluginTcM $ fmap tcl_bndrs getLclEnv
  case location env of
    Nothing -> do
      pprTraceM "got no location for" $ ppr known
      pure []
    Just n -> do
      pprTraceM "solving knowns for" $ ppr env
      hsc <- unsafeTcPluginTcM getTopEnv
      decs <- unsafeTcPluginTcM $ tcg_binds <$> getGblEnv
      anns <- unsafeTcPluginTcM $ tcg_anns <$> getGblEnv
      annenv' <- unsafeTcPluginTcM $ liftIO $ prepareAnnotations hsc Nothing
      let annenv = extendAnnEnvList annenv' anns
      let dec = getVars $ getDec decs n
          z = foldMap (\v -> findAnns (deserializeWithData @TA.Annotation) annenv $ NamedTarget $ getName v) dec
      -- pprTraceM "solving for" $ ppr (n, show z)
      pure $ pure $
        ( EvExpr $
            mkConApp (head $ tyConDataCons $ classTyCon $ tad_knownanns tad)
              [Type (anyTypeOfKind liftedTypeKind) , buildCore tad z]
        , known
        )


parsePromotedAnn :: TransitiveAnnsData -> PredType -> Maybe TA.Annotation
parsePromotedAnn tad ty = do
  (tc, [loc_ty, api_ty, method_ty]) <- splitTyConApp_maybe ty
  guard $ tc == classTyCon (tad_add_ann tad)

  (loc_tc, []) <- splitTyConApp_maybe loc_ty
  loc_dc <- isPromotedDataCon_maybe loc_tc
  guard $ dataConTyCon loc_dc == tad_loc_tc tad
  let loc = toEnum $ dataConTag loc_dc - 1

  api <- fmap unpackFS $ isStrLitTy api_ty
  method <- fmap unpackFS $ isStrLitTy method_ty
  pure $ TA.Annotation loc api method


solveAddAnn :: TransitiveAnnsData -> Ct -> TcPluginM [(EvTerm, Ct)]
solveAddAnn tad to_add = do
  -- pprTraceM "to add" $ ppr $ show $ parsePromotedAnn tad $ ctev_pred $ cc_ev to_add
  pure [(EvExpr $ mkConApp (head $ tyConDataCons $ classTyCon $ tad_add_ann tad) [], to_add)]


getDec :: LHsBinds GhcTc -> Name -> Maybe (HsBindLR GhcTc GhcTc)
getDec bs n = listToMaybe $ do
  L _ b <- bagToList bs
  everything (<>) (mkQ [] $ \case
    FunBind {fun_id = L _ n'}
      | getOccName n == getOccName n' -> [b]
      | otherwise -> []
    (_ :: HsBindLR GhcTc GhcTc) -> []
    ) b

mkString :: String -> Expr Var
mkString str = mkListExpr charTy $ fmap mkCharExpr str

buildCore :: TransitiveAnnsData -> [TA.Annotation] -> Expr Var
buildCore tad anns = mkListExpr (mkTyConTy $ tad_ann_tc tad) $ fmap (buildAnn tad) anns

buildAnn :: TransitiveAnnsData -> TA.Annotation -> CoreExpr
buildAnn tad (TA.Annotation loc s str) = mkConApp (head $ tyConDataCons $ tad_ann_tc tad) $ [mkLoc tad loc, mkString s, mkString str]

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


mkLoc :: TransitiveAnnsData -> TA.Location -> CoreExpr
mkLoc tad loc = mkConApp (tyConDataCons (tad_loc_tc tad) !! fromEnum loc) []


