{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-orphans     #-}

module TransitiveAnns.Plugin where

import Data.Data
import           GHC.Data.Bag (bagToList)
import           GHC.Core.Class (classTyCon)
import           GHC.Tc.Types.Constraint
import           Data.Foldable (fold)
import           Data.IORef (newIORef, modifyIORef', writeIORef, readIORef)
import           Data.Map (Map)
import GHC.Tc.Types.Origin
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC (Class, GhcTc, LHsBindsLR, SrcSpanAnn' (..), HsExpr (..))
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Tc.Types.Evidence (EvTerm(EvExpr))
import           GHC.Tc.Utils.Monad
import           TransitiveAnns.Plugin.Annotations
import           TransitiveAnns.Plugin.Core
import           TransitiveAnns.Plugin.Utils
import qualified TransitiveAnns.Types as TA
import GHC.Hs.Dump
import GHC.Tc.Plugin (newWanted)
import Data.Traversable (for)
import GHC.Hs (HsExpr, LHsExpr)
import Data.Monoid (getFirst, getLast)
import Data.Generics (everything, mkQ)


unsafeAnnsToAddRef :: IORef [Annotation]
unsafeAnnsToAddRef = unsafePerformIO $ newIORef []
{-# NOINLINE unsafeAnnsToAddRef #-}


plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = const $ pure . (CoreDoPluginPass "TransitiveAnns" transann :)
  , tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = do
          unsafeTcPluginTcM $ liftIO $ writeIORef unsafeAnnsToAddRef []
          lookupTransitiveAnnsData
      , tcPluginSolve = solve
      , tcPluginStop = const $ pure ()
      }
  , pluginRecompile  = purePlugin
  }


transann :: ModGuts -> CoreM ModGuts
transann mg = do
  hsc <- getHscEnv
  added <- liftIO $ readIORef unsafeAnnsToAddRef
  annenv' <- liftIO $ prepareAnnotations hsc $ Just mg
  let annenv = extendAnnEnvList annenv' added
  let mganns = mg_anns mg
  let contents = referencedVars $ mg_binds mg
      all_ids = fold contents
      anns = lookupAttachedAnns annenv all_ids
      annotated = propagate contents $ fmap S.fromList anns
      new_anns = buildNewAnnotations (fmap S.toList annotated)

  -- pprTraceM "new anns" $ vcat $ fmap (\(v, s) -> ppr v <+> text (show s)) $ filter (not . null . snd) $ M.assocs annotated
  pure $ mg { mg_anns = mganns <> new_anns <> added }

transitiveAnnEnv :: AnnEnv -> LHsBindsLR GhcTc GhcTc -> AnnEnv
transitiveAnnEnv annenv binds =
  let contents = M.fromList $ mapMaybe (hsBinds . unLoc) $ bagToList binds
      all_ids = fold contents
      anns = lookupAttachedAnns annenv all_ids
      annotated = propagate contents $ fmap S.fromList anns
      new_anns = buildNewAnnotations (fmap S.toList annotated)
   in extendAnnEnvList annenv new_anns

instance {-# OVERLAPPING #-} Outputable (Map Var (Set TA.Annotation)) where
  ppr = vcat . fmap (\(v, s) -> ppr v <+> text (show s)) . M.assocs


findWanted :: Class -> Ct -> Maybe Ct
findWanted c ct = do
  let p = ctev_pred $ cc_ev ct
  case splitTyConApp_maybe p of
    Just (x, _) ->
      case x == classTyCon c of
        True -> Just ct
        False -> Nothing
    _ -> Nothing

-- NOTE(sandy): GHC gives us a wanted, even when we have a given in scope, in
-- case we'd like to just solve it. But we don't!
--
-- So instead, we would like to solve AddAnns only when there is no AddAnns as
-- a given.
solve :: TransitiveAnnsData -> TcPluginSolver
solve tad gs ds ws' = do
  let ws = ws' <> ds
  let over k f =
        let fgs = mapMaybe (findWanted $ f tad) gs
            fdws = mapMaybe (findWanted $ f tad) ws
          in if null fgs && not (null fdws)
                then do
                  -- pprTraceM "ws" $ ppr fdws
                  -- pprTraceM "spans ws" $ ppr $ fmap (ctLocSpan . ctLoc) fdws
                  traverse (k tad) fdws
                else pure []
  -- pprTraceM "ws/gs" $ ppr (gs, ws)
  adds   <- over solveAddAnn    tad_add_ann
  (has_ev, has_new)   <- fmap unzip $ over solveToHasAnns tad_to_has_ann
  knowns <- over solveKnownAnns tad_knownanns
  let res = concat $ adds <> knowns
  pure $ TcPluginOk (res <> has_ev) $ concat has_new


solveToHasAnns :: TransitiveAnnsData -> Ct -> TcPluginM ((EvTerm, Ct), [Ct])
solveToHasAnns tad ct = do
  decs <- unsafeTcPluginTcM $ tcg_binds <$> getGblEnv
  let  !expr = getCallingExpr (location ct) decs
  anns <- unsafeTcPluginTcM $ getReferencedAnnotations expr
  pprTraceM "found_anns" $ text $ show anns
  new_cts <- for anns $ fmap mkNonCanonical . newWanted (ctLoc ct) . toHasAnn tad
  pprTraceM "new_cts" $ ppr new_cts
  pure
    ( (, ct) $ EvExpr $ mkToHasAnnsDict tad
    , new_cts
    )


getCallingExpr :: Data a => RealSrcSpan -> a -> Maybe (LHsExpr GhcTc)
getCallingExpr ss a = getLast $ everything (<>) (mkQ mempty
  $ \case
      x@(L _ (HsApp _ (L (SrcSpanAnn _ (RealSrcSpan ss' _)) _) b))
        | ss' == ss -> pprTrace "contains!" (ppr x) $ pure b
      x@(L _ (OpApp _ (L (SrcSpanAnn _ (RealSrcSpan ss' _)) _) _ b))
        | ss' == ss -> pprTrace "contains!" (ppr x) $ pure b
      x@(L _ (HsApp _ (L (SrcSpanAnn _ (RealSrcSpan ss' _)) _) b))
        | ss' == ss -> pprTrace "contains!" (ppr x) $ pure b
      x@(L (SrcSpanAnn _ (RealSrcSpan ss' _)) _)
        | containsSpan ss' ss -> pprTrace "contains..." (ppr ((ss, ss'), x, showAstData NoBlankSrcSpan NoBlankEpAnnotations x)) mempty
      (_ :: LHsExpr GhcTc) -> mempty

  ) a



getAttachedAnnotations :: Ct -> TcM [TA.Annotation]
getAttachedAnnotations known = do
  hsc <- getTopEnv
  anns <- tcg_anns <$> getGblEnv
  annenv' <- liftIO $ prepareAnnotations hsc Nothing
  added <- liftIO $ readIORef unsafeAnnsToAddRef
  let annenv'' = extendAnnEnvList annenv' $ anns <> added
  decs <- tcg_binds <$> getGblEnv
  let Just (nm, dec) = getDec decs $ location known
      vars = S.insert nm $ getVars dec
  let annenv = transitiveAnnEnv annenv'' decs
  pure $ foldMap (findAnns (deserializeWithData @TA.Annotation) annenv . NamedTarget . getName) vars


-- TODO(sandy): dedup me wrt getAttachedAnnotations
getReferencedAnnotations :: Data a => a -> TcM [TA.Annotation]
getReferencedAnnotations a = do
  hsc <- getTopEnv
  anns <- tcg_anns <$> getGblEnv
  annenv' <- liftIO $ prepareAnnotations hsc Nothing
  added <- liftIO $ readIORef unsafeAnnsToAddRef
  let annenv'' = extendAnnEnvList annenv' $ anns <> added
  decs <- tcg_binds <$> getGblEnv
  let vars = getVars a
  pprTraceM "contains vars" $ ppr vars
  let annenv = transitiveAnnEnv annenv'' decs
  pure $ foldMap (findAnns (deserializeWithData @TA.Annotation) annenv . NamedTarget . getName) vars

solveKnownAnns :: TransitiveAnnsData -> Ct -> TcPluginM [(EvTerm, Ct)]
solveKnownAnns tad known = do
  z <- unsafeTcPluginTcM $ getAttachedAnnotations known
  pure $ pure (EvExpr $ mkKnownAnnsDict tad z, known)


solveAddAnn :: TransitiveAnnsData -> Ct -> TcPluginM [(EvTerm, Ct)]
solveAddAnn tad to_add
  | Just ann <- parsePromotedAnn tad $ ctev_pred $ cc_ev to_add
  -- , Just ctloc <- location to_add
  = do
    decs <- unsafeTcPluginTcM $ tcg_binds <$> getGblEnv
    Just dec <- pure $ fmap fst $ getDec decs $ location to_add
    -- pprTraceM "solved with" $ ppr (dec, text $ show ann)
    let annx = Annotation (NamedTarget $ getName dec) (toSerialized serializeWithData ann)
    -- pprTraceM "annx" $ ppr annx
    unsafeTcPluginTcM
      $ liftIO
      $ modifyIORef' unsafeAnnsToAddRef
      $ (annx :)
    pure $ pure (EvExpr $ mkAddAnnDict tad ann, to_add)
  | otherwise = pure []

