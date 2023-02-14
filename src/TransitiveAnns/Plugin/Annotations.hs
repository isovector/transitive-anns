{-# LANGUAGE ViewPatterns #-}

module TransitiveAnns.Plugin.Annotations
  ( propagate
  , getVars
  , lookupAttachedAnns
  , buildNewAnnotations
  , referencedVars
  , hsBinds
  ) where

import           Data.Data hiding (TyCon)
import           Data.Functor ((<&>))
import           Data.Generics (everything, mkQ)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MM
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC.Hs hiding (anns)
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import qualified TransitiveAnns.Types as TA


------------------------------------------------------------------------------
-- | Get the name and set of referenced variables for a binding.
hsBinds :: HsBindLR GhcTc GhcTc -> Maybe (Var, Set Var)
hsBinds (FunBind _ (L _ gl) mg _) = Just (gl, getVars mg)
hsBinds PatBind{} = Nothing
hsBinds VarBind{} = Nothing
hsBinds (AbsBinds _ _ _ [(ABE _ nm _ _ _)] _ bag _) = Just (nm, getVars bag)
hsBinds (AbsBinds _ _ _ _ _ _ _) = Nothing
hsBinds PatSynBind{} = Nothing


------------------------------------------------------------------------------
-- | Apply a function over all binds, accumulating the results into a map.
forBinds :: Ord b => (Expr b -> r) -> Bind b -> Map b r
forBinds f (NonRec b ex) = M.singleton b $ f ex
forBinds f (Rec x0) = foldMap (\(b, e) -> M.singleton b $ f e) x0


------------------------------------------------------------------------------
-- | Given an annotation environment and set of binders, find every annotation
-- attached directly to the the binder.
lookupAttachedAnns :: AnnEnv -> Set CoreBndr -> Map CoreBndr [TA.Annotation]
lookupAttachedAnns annenv = foldMap $ \b ->
  M.singleton b
    $ findAnns (deserializeWithData @TA.Annotation) annenv
    $ NamedTarget
    $ getName b


------------------------------------------------------------------------------
-- | Get a map of every referenced variable for each binding.
referencedVars :: (Foldable t, Ord b, Data b) => t (Bind b) -> Map b (Set Var)
referencedVars bs = flip foldMap bs $ forBinds getVars


------------------------------------------------------------------------------
-- | Find every variable in an expression. Used to determine which variables
-- are referenced for transitivity propagation.
getVars :: Data a => a -> Set Var
getVars = everything (<>) $ mkQ mempty S.singleton


------------------------------------------------------------------------------
-- | Turn a map of binders to 'TA.Annotation's into real, GHC-worthy
-- annotations.
buildNewAnnotations :: Map CoreBndr [TA.Annotation] -> [Annotation]
buildNewAnnotations annotated = do
  (b, as) <- M.toList annotated
  as <&> \a ->
    Annotation (NamedTarget $ getName b) $ toSerialized serializeWithData a


------------------------------------------------------------------------------
-- | @'propagate' edges refs@ computes the transitive references reachable from
-- every node by following the edges.
propagate :: (Ord a, Ord b) => Map a (Set a) -> Map a (Set b) -> Map a (Set b)
propagate (toMonoidal -> refs)
  = fromMonoidal . fixEq (inherit refs) . MM.filter (not . null) . toMonoidal

------------------------------------------------------------------------------
-- | One step of the 'propagate' mechanism.
inherit
    :: (Ord a, Ord b)
    => MonoidalMap a (Set a)
    -> MonoidalMap a (Set b)
    -> MonoidalMap a (Set b)
inherit refs anns = flip MM.foldMapWithKey refs $ \k as ->
  flip foldMap as $ \a2 ->
    foldMap (MM.singleton k) $ MM.lookup a2 anns

------------------------------------------------------------------------------
-- | Find a fixpoint of the given function, appending its results until no
-- further progress is made.
fixEq :: (Semigroup d, Eq d) => (d -> d) -> d -> d
fixEq f a =
  let b = a <> f a in
  if b == a then a else fixEq f b


------------------------------------------------------------------------------
-- | Convert a Map into a MonoidalMap
toMonoidal :: Ord a => Map a b -> MonoidalMap a b
toMonoidal = MM.fromList . M.toList


------------------------------------------------------------------------------
-- | Convert a MonoidalMap into a Map
fromMonoidal :: Ord a => MonoidalMap a b -> Map a b
fromMonoidal = M.fromList . MM.toList

