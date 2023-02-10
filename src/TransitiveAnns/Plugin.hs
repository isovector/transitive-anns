{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TransitiveAnns.Plugin where

import           GHC.Data.Bag (bagToList)
import           GHC.Core.Class (classTyCon)
import           GHC.Tc.Types.Constraint
import           Data.Foldable (fold)
import           Data.IORef (newIORef, modifyIORef', writeIORef, readIORef)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC (Class, GhcTc, LHsBindsLR)
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Tc.Types.Evidence (EvTerm(EvExpr))
import           GHC.Tc.Utils.Monad
import           TransitiveAnns.Plugin.Annotations
import           TransitiveAnns.Plugin.Core
import           TransitiveAnns.Plugin.Utils
import qualified TransitiveAnns.Types as TA


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

pprTraceId :: Outputable a => String -> a -> a
pprTraceId x a = pprTrace x (ppr a) a

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


solve :: TransitiveAnnsData -> TcPluginSolver
solve tad _ ds ws = do
  pprTraceM "all derived" $ ppr ds
  -- pprTraceM "all wanted topdecs" $ ppr $ fmap location ws
  let over k f = traverse (k tad) $ mapMaybe (findWanted $ f tad) $ ds <> ws
  adds   <- over solveAddAnn    tad_add_ann
  knowns <- over solveKnownAnns tad_knownanns
  let res = concat $ adds <> knowns
  pure $ TcPluginOk res []


solveKnownAnns :: TransitiveAnnsData -> Ct -> TcPluginM [(EvTerm, Ct)]
solveKnownAnns tad known
  | Just loc <- location known
  = do
      hsc <- unsafeTcPluginTcM getTopEnv
      anns <- unsafeTcPluginTcM $ tcg_anns <$> getGblEnv
      annenv' <- unsafeTcPluginTcM $ liftIO $ prepareAnnotations hsc Nothing
      added <- unsafeTcPluginTcM $ liftIO $ readIORef unsafeAnnsToAddRef
      -- pprTraceM "\n-----\n----" $ ppr loc
      -- pprTraceM "added during known" $ ppr added
      let annenv'' = extendAnnEnvList annenv' $ anns <> added
      decs <- unsafeTcPluginTcM $ tcg_binds <$> getGblEnv
      let Just (nm, dec) = getDec decs loc
          vars = S.insert nm $ getVars dec
      let annenv = transitiveAnnEnv annenv'' decs
          z = foldMap (findAnns (deserializeWithData @TA.Annotation) annenv . NamedTarget . getName) vars

      -- pprTraceM "solving for" $ ppr (ppr (fmap (fmap getVars) $ getDec decs loc, text $ show z))
      pure $ pure (EvExpr $ mkKnownAnnsDict tad z, known)
    | otherwise = pure []


solveAddAnn :: TransitiveAnnsData -> Ct -> TcPluginM [(EvTerm, Ct)]
solveAddAnn tad to_add
  | Just ann <- parsePromotedAnn tad $ ctev_pred $ cc_ev to_add
  , Just ctloc <- location to_add
  = do
    decs <- unsafeTcPluginTcM $ tcg_binds <$> getGblEnv
    Just dec <- pure $ fmap fst $ getDec decs ctloc
    let annx = Annotation (NamedTarget $ getName dec) (toSerialized serializeWithData ann)
    -- pprTraceM "annx" $ ppr annx
    unsafeTcPluginTcM
      $ liftIO
      $ modifyIORef' unsafeAnnsToAddRef
      $ (annx :)
    pure $ pure (EvExpr $ mkAddAnnDict tad ann, to_add)
  | otherwise = pure []

