{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module TransitiveAnns.Plugin where

import           Bag (bagToList)
import           Class (classTyCon)
import           Constraint
import           Data.Data hiding (TyCon)
import           Data.Foldable (fold)
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
  , tad_ann_tc :: TyCon
  , tad_loc_tc :: TyCon
  }


------------------------------------------------------------------------------
-- | Lookup the classes from 'Data.Constraint.Emerge' and build an
-- 'EmergeData'.
lookupTransitiveAnnssData :: TcPluginM TransitiveAnnsData
lookupTransitiveAnnssData = do
    Found _ md  <- findImportedModule modul Nothing
    emergeTcNm  <- lookupOrig md $ mkTcOcc "KnownAnnotations"
    ann  <- lookupOrig md $ mkTcOcc "Annotation"
    loc  <- lookupOrig md $ mkTcOcc "Location"

    TransitiveAnnsData
        <$> tcLookupClass emergeTcNm
        <*> tcLookupTyCon ann
        <*> tcLookupTyCon loc
  where
    modul  = mkModuleName "TransitiveAnns.Types"


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
  pure $ mg { mg_anns = new_anns ++ mganns }

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
      , tcPluginSolve = solveKnownAnns

      , tcPluginStop = const $ pure ()
      }
  , pluginRecompile  = purePlugin
  }

findEmergePred :: Class -> Ct -> Maybe Ct
findEmergePred c ct = do
  let p = ctev_pred $ cc_ev ct
  case splitTyConApp_maybe p of
    Just (x, _) ->
      case x == classTyCon c of
        True -> Just ct
        False -> Nothing
    _ -> Nothing


solveKnownAnns :: TransitiveAnnsData -> TcPluginSolver
solveKnownAnns tad _ _ ws
  | [known] <- mapMaybe (findEmergePred (tad_knownanns tad)) ws
  = do
    env <- unsafeTcPluginTcM $ fmap tcl_bndrs getLclEnv
    case location env of
      Nothing -> pure $ TcPluginOk [] []
      Just n -> do
        hsc <- unsafeTcPluginTcM getTopEnv
        decs <- unsafeTcPluginTcM $ tcg_binds <$> getGblEnv
        anns <- unsafeTcPluginTcM $ tcg_anns <$> getGblEnv
        annenv' <- unsafeTcPluginTcM $ liftIO $ prepareAnnotations hsc Nothing
        let annenv = extendAnnEnvList annenv' anns
        let dec = getVars $ getDec decs n
            z = foldMap (\v -> findAnns (deserializeWithData @TA.Annotation) annenv $ NamedTarget $ getName v) dec
        pure $ TcPluginOk [(EvExpr $ mkConApp (head $ tyConDataCons $ classTyCon $ tad_knownanns tad) $ pure $ buildCore tad z, known)] []
  | otherwise = pure $ TcPluginOk [] []

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

mkLoc :: TransitiveAnnsData -> TA.Location -> CoreExpr
mkLoc tad loc = mkConApp (tyConDataCons (tad_loc_tc tad) !! fromEnum loc) []

