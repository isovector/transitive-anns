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

