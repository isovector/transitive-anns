{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin where

import           Ann (Ann (Ann))
import           Data.Data hiding (TyCon)
import           Data.Foldable (fold)
import           Data.Functor ((<&>))
import           Data.Generics (everything, mkQ)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import TcRnMonad

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins hiding ((<>), empty)
#else
import GhcPlugins hiding (TcPlugin, (<>), empty)
#endif

import Constraint
import GHC (LHsBinds, GhcTc, HsBindLR, Class)
import Bag (bagToList)
import GHC.Hs.Binds
import Control.Applicative (empty)
import GHC.Hs.Dump (showAstData, BlankSrcSpan (BlankSrcSpan))
import TcPluginM (findImportedModule, lookupOrig, tcLookupClass, tcLookupTyCon, tcLookupDataCon)
import Class (classTyCon)
import TcEvidence (EvTerm(EvExpr))

------------------------------------------------------------------------------

instance Outputable Ann where
  ppr (Ann api call) = ppr (api, call)

type AnnToTrack = Ann

forBinds :: Ord b => (Expr b -> r) -> Bind b -> Map b r
forBinds f (NonRec b ex) = M.singleton b $ f ex
forBinds f (Rec x0) = foldMap (\(b, e) -> M.singleton b $ f e) x0

lookupAttachedAnns :: AnnEnv -> Set CoreBndr -> Map CoreBndr [AnnToTrack]
lookupAttachedAnns annenv = foldMap $ \b ->
  M.singleton b $ findAnns (deserializeWithData @AnnToTrack) annenv $ NamedTarget $ getName b

referencedVars :: [Bind CoreBndr] -> Map CoreBndr (Set CoreBndr)
referencedVars bs = flip foldMap bs $ forBinds getVars

getVars :: Data a => a -> Set Var
getVars =
  everything (<>) $ mkQ mempty S.singleton

withAnnotations :: Ord b => Map b [c] -> Map a (Set b) -> Map a [c]
withAnnotations anns cs = M.map (foldMap (fromMaybe [] . flip M.lookup anns)) cs

buildNewAnnotations :: Map CoreBndr [AnnToTrack] -> [Annotation]
buildNewAnnotations annotated = do
  (b, as) <- M.toList annotated
  as <&> \a -> Annotation (NamedTarget $ getName b) $ toSerialized serializeWithData a


data TransAnnData = TransAnnData
  { tad_knownanns :: Class
  , tad_ann_tc :: TyCon
  }


------------------------------------------------------------------------------
-- | Lookup the classes from 'Data.Constraint.Emerge' and build an
-- 'EmergeData'.
lookupTransAnnsData :: TcPluginM TransAnnData
lookupTransAnnsData = do
    Found _ md  <- findImportedModule emergeModule Nothing
    Found _ md2  <- findImportedModule annModule Nothing
    emergeTcNm  <- lookupOrig md $ mkTcOcc "KnownAnns"
    ann  <- lookupOrig md2 $ mkTcOcc "Ann"

    TransAnnData
        <$> tcLookupClass emergeTcNm
        <*> tcLookupTyCon ann
  where
    emergeModule  = mkModuleName "KnownAnns"
    annModule  = mkModuleName "Ann"


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
  { installCoreToDos = const $ pure . (CoreDoPluginPass "TransAnn" transann :)
  , tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = lookupTransAnnsData
      , tcPluginSolve = solveKnownAnns

      , tcPluginStop = const $ pure ()
      }
  , pluginRecompile  = purePlugin
  }

findEmergePred :: Class -> Ct -> Maybe Ct
findEmergePred c ct = do
  let pred = ctev_pred $ cc_ev ct
  case splitTyConApp_maybe pred of
    Just (x, _) ->
      case x == classTyCon c of
        True -> Just (ct)
        False -> Nothing
    _ -> Nothing


solveKnownAnns :: TransAnnData -> TcPluginSolver
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
            z = foldMap (\v -> findAnns (deserializeWithData @AnnToTrack) annenv $ NamedTarget $ getName v) dec
        pprTraceM "decs" $ ppr z
        pure $ TcPluginOk [(EvExpr $ buildCore tad z, known)] []
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

buildCore :: TransAnnData -> [Ann] -> Expr Var
buildCore tad anns = mkListExpr (mkTyConTy $ tad_ann_tc tad) $ fmap (buildAnn tad) anns

buildAnn :: TransAnnData -> Ann -> CoreExpr
buildAnn tad (Ann s str) = mkCoreConApps (head $ tyConDataCons $ tad_ann_tc tad) $ [mkString s, mkString str]

