{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module TransitiveAnns.Plugin.Annotations where

import           Data.Data hiding (TyCon)
import           Data.Functor ((<&>))
import           Data.Generics (everything, mkQ)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MM
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC.Hs
import           GhcPlugins hiding (TcPlugin, (<>), empty)
import qualified TransitiveAnns.Types as TA


hsBinds :: HsBindLR GhcTc GhcTc -> Maybe (Var, Set Var)
hsBinds (FunBind _ (L _ gl) mg _ _) = Just (gl, getVars mg)
hsBinds PatBind{} = Nothing
hsBinds VarBind{} = Nothing
hsBinds (AbsBinds _ _ _ [(ABE _ nm _ _ _)] _ bag _) = Just (nm, getVars bag)
hsBinds (AbsBinds _ _ _ _ _ _ _) = Nothing
hsBinds PatSynBind{} = Nothing
hsBinds XHsBindsLR{} = Nothing

forBinds :: Ord b => (Expr b -> r) -> Bind b -> Map b r
forBinds f (NonRec b ex) = M.singleton b $ f ex
forBinds f (Rec x0) = foldMap (\(b, e) -> M.singleton b $ f e) x0


lookupAttachedAnns :: AnnEnv -> Set CoreBndr -> Map CoreBndr [TA.Annotation]
lookupAttachedAnns annenv = foldMap $ \b ->
  M.singleton b $ findAnns (deserializeWithData @TA.Annotation) annenv $ NamedTarget $ getName b


referencedVars :: (Foldable t, Ord b, Data b) => t (Bind b) -> Map b (Set Var)
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


inherit :: (Ord a, Ord b) => MonoidalMap a (Set a) -> MonoidalMap a (Set b) -> MonoidalMap a (Set b)
inherit refs anns = flip MM.foldMapWithKey refs $ \k as ->
  flip foldMap as $ \a2 ->
    foldMap (MM.singleton k) $ MM.lookup a2 anns
  -- fix $ \final -> M.fromList $ do
  --   (from, to_set) <- M.assocs refs
  --   to <- S.toList to_set
  --   let transitive = fromMaybe (fold $ M.lookup to anns)  $ M.lookup to final
  --   pure (from, transitive)

toMonoidal :: Ord a => Map a b -> MonoidalMap a b
toMonoidal = MM.fromList . M.toList

fromMonoidal :: Ord a => MonoidalMap a b -> Map a b
fromMonoidal = M.fromList . MM.toList

propagate :: (Ord a, Ord b) => Map a (Set a) -> Map a (Set b) -> Map a (Set b)
propagate (toMonoidal -> refs) = fromMonoidal . fixEq (inherit refs) . MM.filter (not . null) . toMonoidal


addMore :: (Ord b, Ord a) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
addMore = M.unionWith (<>)

fixEq :: (Semigroup d, Eq d) => (d -> d) -> d -> d
fixEq f a =
  let b = a <> f a in
  if b == a then a else fixEq f b

