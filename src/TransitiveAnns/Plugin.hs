{-# LANGUAGE LambdaCase #-}

module TransitiveAnns.Plugin where

import           Data.Data
import           Data.Foldable (fold)
import           Data.IORef (newIORef, modifyIORef', writeIORef, readIORef)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import           Data.Traversable (for)
import           GHC (Class, GhcTc, LHsBindsLR)
import           GHC.Core.Class (classTyCon)
import           GHC.Data.Bag (bagToList)
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import           GHC.Tc.Types.Constraint
import           GHC.Tc.Types.Evidence (EvTerm(EvExpr))
import           GHC.Tc.Utils.Monad
import           GHC.Tc.Utils.TcMType (newWanted)
import           System.IO.Unsafe (unsafePerformIO)
import           TransitiveAnns.Plugin.Annotations
import           TransitiveAnns.Plugin.Core
import           TransitiveAnns.Plugin.Utils
import qualified TransitiveAnns.Types as TA


------------------------------------------------------------------------------
-- | The entrypoint to our plugin. It installs a TC plugin to solve the magic
-- typeclasses, and a core plugin to propagate annotations.
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = const $ pure . (CoreDoPluginPass "TransitiveAnns" corePlugin :)
  , tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = do
          unsafeTcPluginTcM $ liftIO $ writeIORef unsafeAnnsToAddRef []
          lookupTransitiveAnnsData
      , tcPluginSolve = solve
      , tcPluginStop = const $ pure ()
      }
  , pluginRecompile  = purePlugin
  }

------------------------------------------------------------------------------
-- | GHC gives us no way to add annotations in the course of a TC plugin,
-- although we can do it in a core plugin. So instead, we make this
-- bgamari-approved IORef to move data from the TC plugin pass to the core
-- plugin pass.
unsafeAnnsToAddRef :: IORef [Annotation]
unsafeAnnsToAddRef = unsafePerformIO $ newIORef []
{-# NOINLINE unsafeAnnsToAddRef #-}


------------------------------------------------------------------------------
-- | The core plugin transitively propagates annotations from any referenced
-- variable to the referencing variable.
corePlugin :: ModGuts -> CoreM ModGuts
corePlugin mg = do
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
  pure $ mg { mg_anns = mganns <> new_anns <> added }


------------------------------------------------------------------------------
-- | Given an annotation environment and some bindings, compute the transitive
-- annotation environment after propagating all referenced annotations upwards.
-- This is equivalent to what the core plugin does, but needs to run during
-- typechecking if we are trying to solve the known annotations *in the current
-- module*.
transitiveAnnEnv :: AnnEnv -> LHsBindsLR GhcTc GhcTc -> AnnEnv
transitiveAnnEnv annenv binds =
  let contents = M.fromList $ mapMaybe (hsBinds . unLoc) $ bagToList binds
      all_ids = fold contents
      anns = lookupAttachedAnns annenv all_ids
      annotated = propagate contents $ fmap S.fromList anns
      new_anns = buildNewAnnotations (fmap S.toList annotated)
   in extendAnnEnvList annenv new_anns


------------------------------------------------------------------------------
-- | Determine if the given constraint is of the given class.
findWanted :: Class -> Ct -> Maybe Ct
findWanted c ct = do
  let p = ctev_pred $ cc_ev ct
  case splitTyConApp_maybe p of
    Just (x, _) ->
      case x == classTyCon c of
        True -> Just ct
        False -> Nothing
    _ -> Nothing


------------------------------------------------------------------------------
-- | The entry-point to the TC plugin. This is responsible for solving
-- AddAnnotation, KnownAnnotation and ToHasAnnotations nstraints
solve :: TransitiveAnnsData -> TcPluginSolver
solve tad gs ds ws' = do
  -- Our to-solve wanteds might have been derived, so add the deriveds to the
  -- wanteds.
  let ws = ws' <> ds
  -- Because of our skolem trick, GHC doesn't recognize that we'd like to solve
  -- a wanted with a given. This function only attemps to solve the class if
  -- there are no givens for the class in scope.
  let over k f =
        let fgs = mapMaybe (findWanted $ f tad) gs
            fdws = mapMaybe (findWanted $ f tad) ws
          in if null fgs && not (null fdws)
                then traverse (k tad) fdws
                else pure []

  -- Try to solve the three classes
  adds <- over solveAddAnn tad_add_ann
  (has_ev, has_new)
    <- fmap unzip $ over solveToHasAnns tad_to_has_ann
  knowns <- over solveKnownAnns tad_knownanns
  let res = concat $ adds <> knowns
  pure $ TcPluginOk (res <> has_ev) $ concat has_new


------------------------------------------------------------------------------
-- | "Solve" a ToHasAnnotations wanted by rewriting it as HasAnnotation wanteds
-- for every annotation attached to the given expression.
solveToHasAnns :: TransitiveAnnsData -> Ct -> TcPluginM ((EvTerm, Ct), [Ct])
solveToHasAnns tad ct = unsafeTcPluginTcM $ do
  decs <- tcg_binds <$> getGblEnv
  let expr = getCallingExpr (location ct) decs
  anns <- getReferencedAnnotations expr
  -- NOTE: newWanted has a bug where it destroys source information. See GHC
  -- #20895 for why we need to call 'setCtLocM' here.
  new_cts <- setCtLocM (ctLoc ct)
          $ for anns
          $ fmap mkNonCanonical
              . newWanted (ctLocOrigin $ ctLoc ct) Nothing
              . toHasAnn tad
  pure
    ( (EvExpr $ mkToHasAnnsDict tad, ct)
    , new_cts
    )

------------------------------------------------------------------------------
-- | Get all annotations attached to the top-level binding that gave rise to
-- the given constraint. Due to transitivity, these semantics necessarily mean
-- we lose referential transparency. It's a bad thing, which is why
-- 'getReferencedAnnotations' exists.
--
-- TODO(sandy): Remove this and fix the semantics for KnownAnnotations.
getAttachedAnnotations :: Ct -> TcM [TA.Annotation]
getAttachedAnnotations known = do
  hsc <- getTopEnv
  anns <- tcg_anns <$> getGblEnv
  annenv' <- liftIO $ prepareAnnotations hsc Nothing
  added <- liftIO $ readIORef unsafeAnnsToAddRef
  let annenv'' = extendAnnEnvList annenv' $ anns <> added
  decs <- tcg_binds <$> getGblEnv
  (nm, dec) <-
    maybe
        (pprPanic "unable to find corresponding definition for" $ ppr known)
        pure
      $ getDec decs $ location known
  let vars = S.insert nm $ getVars dec
  let annenv = transitiveAnnEnv annenv'' decs
  pure
    $ foldMap
        (findAnns (deserializeWithData @TA.Annotation) annenv
          . NamedTarget
          . getName
        ) vars


------------------------------------------------------------------------------
-- | Given some term, find every reachable annotation. This should be used with
-- the result of getCallingExpr.
getReferencedAnnotations :: Data a => a -> TcM [TA.Annotation]
getReferencedAnnotations a = do
  hsc <- getTopEnv
  anns <- tcg_anns <$> getGblEnv
  annenv' <- liftIO $ prepareAnnotations hsc Nothing
  added <- liftIO $ readIORef unsafeAnnsToAddRef
  let annenv'' = extendAnnEnvList annenv' $ anns <> added
  decs <- tcg_binds <$> getGblEnv
  let vars = getVars a
  let annenv = transitiveAnnEnv annenv'' decs
  pure $
    foldMap
      (findAnns (deserializeWithData @TA.Annotation) annenv
        . NamedTarget
        . getName
      ) vars


------------------------------------------------------------------------------
-- | Solve a KnownAnnotations constraint by inlining every annotation attached
-- to the current definition.
solveKnownAnns :: TransitiveAnnsData -> Ct -> TcPluginM [(EvTerm, Ct)]
solveKnownAnns tad known = do
  z <- unsafeTcPluginTcM $ getAttachedAnnotations known
  pure $ pure (EvExpr $ mkKnownAnnsDict tad z, known)


------------------------------------------------------------------------------
-- | Solve an AddAnnotation constraint by adding it to the list of annotations
-- that we will install during the core plugin.
solveAddAnn :: TransitiveAnnsData -> Ct -> TcPluginM [(EvTerm, Ct)]
solveAddAnn tad to_add
  | Just ann <- parsePromotedAnn tad $ ctev_pred $ cc_ev to_add
  = do
    decs <- unsafeTcPluginTcM $ tcg_binds <$> getGblEnv
    Just dec <- pure $ fmap fst $ getDec decs $ location to_add
    let annx = Annotation (NamedTarget $ getName dec)
             $ toSerialized serializeWithData ann
    unsafeTcPluginTcM
      $ liftIO
      $ modifyIORef' unsafeAnnsToAddRef
      $ (annx :)
    pure $ pure (EvExpr $ mkAddAnnDict tad ann, to_add)
  | otherwise = pure []

