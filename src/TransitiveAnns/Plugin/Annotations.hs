{-# LANGUAGE TypeApplications #-}

module TransitiveAnns.Plugin.Annotations where

import           Data.Data hiding (TyCon)
import           Data.Functor ((<&>))
import           Data.Generics (everything, mkQ)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           GhcPlugins hiding (TcPlugin, (<>), empty)
import qualified TransitiveAnns.Types as TA


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


inherit :: Ord a => Map a (Set a) -> Map a (Set b) -> Map a (Set b)
inherit refs anns = flip M.foldMapWithKey refs $  \k as ->
  flip foldMap as $ \a2 ->
    foldMap (M.singleton k) $ M.lookup a2 anns
  -- fix $ \final -> M.fromList $ do
  --   (from, to_set) <- M.assocs refs
  --   to <- S.toList to_set
  --   let transitive = fromMaybe (fold $ M.lookup to anns)  $ M.lookup to final
  --   pure (from, transitive)

propagate :: (Ord a, Ord b) => Map a (Set a) -> Map a (Set b) -> Map a (Set b)
propagate refs = fixEq (inherit refs) . M.filter (not . null)


addMore :: (Ord b, Ord a) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
addMore = M.unionWith (<>)

fixEq :: (Semigroup d, Eq d) => (d -> d) -> d -> d
fixEq f a =
  let b = a <> f a in
  if b == a then a else fixEq f b

-- test_contents :: Map [Char] (Set [Char])
-- test_contents =
--   M.fromList [("$dEq",S.fromList ["$fEqAnnotation","$fEqSet"]),("$dShow",S.fromList ["$fShowAnnotation","$fShowSet"]),("$trModule",S.fromList ["Module","TrNameS"]),("r1a",S.fromList ["[]","annotated","r1"]),("r2a",S.fromList ["[]","annotated","r2"]),("r3a",S.fromList ["[]","annotated","r3"]),("r4a",S.fromList ["[]","annotated","r4"]),("r5a",S.fromList ["[]","annotated","r5"]),("r6a",S.fromList [":","Annotation","C#","Local","[]","annotated","r6"]),("spec",S.fromList ["$","$dEq","$dShow","$fExampleIO","$fMonadSpecM","$fOrdAnnotation","(,)",">>","Annotation","I#","Local","SrcLoc","a","ann","build","c","describe","emptyCallStack","S.fromList","it","n","pushCallStack","r1a","r2a","r3a","r4a","r5a","r6a","shouldBe","unpackCString#"])]

-- test_anns :: Map [Char] (Set Bool)
-- test_anns =
--     M.fromList [("$",S.fromList []),("$dEq",S.fromList []),("$dShow",S.fromList []),("$fEqAnnotation",S.fromList []),("$fEqSet",S.fromList []),("$fExampleIO",S.fromList []),("$fMonadSpecM",S.fromList []),("$fOrdAnnotation",S.fromList []),("$fShowAnnotation",S.fromList []),("$fShowSet",S.fromList []),("(,)",S.fromList []),(":",S.fromList []),(">>",S.fromList []),("Annotation",S.fromList []),("C#",S.fromList []),("I#",S.fromList []),("Local",S.fromList []),("Module",S.fromList []),("SrcLoc",S.fromList []),("TrNameS",S.fromList []),("[]",S.fromList []),("a",S.fromList []),("ann",S.fromList []),("annotated",S.fromList []),("build",S.fromList []),("c",S.fromList []),("describe",S.fromList []),("emptyCallStack",S.fromList []),("S.fromList",S.fromList []),("it",S.fromList []),("n",S.fromList []),("pushCallStack",S.fromList []),("r1",S.fromList []),("r1a",S.fromList []),("r2",S.fromList []),("r2a",S.fromList []),("r3",S.fromList []),("r3a",S.fromList []),("r4",S.fromList []),("r4a",S.fromList []),("r5",S.fromList []),("r5a",S.fromList []),("r6",S.fromList [True]),("r6a",S.fromList []),("shouldBe",S.fromList []),("unpackCString#",S.fromList [])]

-- test =
--   -- propagate test_contents $ M.filter (not . null) test_anns
--   propagate
--     (M.fromList
--       [ ("r1", S.fromList ["r2"])
--       , ("r0", S.fromList ["r6"])
--       ])
--     (M.fromList
--       [ ("r6", S.fromList [True])
--       , ("x", S.fromList [])
--       ]
--     )

