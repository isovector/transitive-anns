module TransitiveAnns.Plugin.Core where

import           Data.String (fromString)
import           GHC.Core.Class (Class)
import           GHC.Plugins hiding (TcPlugin, (<>), empty)
import           TransitiveAnns.Plugin.Utils
import qualified TransitiveAnns.Types as TA

------------------------------------------------------------------------------
-- | Make a string literal in core. Leads to less-efficient core than the
-- version shipped by GHC, but can be done without a MonadThings instance.
mkStrLit :: String -> Expr Var
mkStrLit str = mkListExpr charTy $ fmap mkCharExpr str


------------------------------------------------------------------------------
-- | Emit core for a literal 'TA.Location'.
mkLoc :: TransitiveAnnsData -> TA.Location -> CoreExpr
mkLoc tad loc = mkConApp (tyConDataCons (tad_loc_tc tad) !! fromEnum loc) []


------------------------------------------------------------------------------
-- | Make a core expression for the only data constructor of the given tycon.
mkOnlyDataCon :: TyCon -> [CoreExpr] -> CoreExpr
mkOnlyDataCon = mkConApp . head . tyConDataCons


------------------------------------------------------------------------------
-- | Make a core expression for a class.
mkClassDict :: Class -> [CoreExpr] -> CoreExpr
mkClassDict = mkConApp . classDataCon


------------------------------------------------------------------------------
-- | Construct a literal 'TA.Annotation'.
mkAnnotation :: TransitiveAnnsData -> TA.Annotation -> CoreExpr
mkAnnotation tad (TA.Annotation loc s str) = mkOnlyDataCon (tad_ann_tc tad)
  [ mkLoc tad loc
  , mkStrLit s
  , mkStrLit str
  ]


------------------------------------------------------------------------------
-- | Core for @GHC.Exts.Any :: Type@. Used to fill in skolems that exist only
-- to prevent GHC from caching our solved constraints.
typeAny :: Type
typeAny = anyTypeOfKind liftedTypeKind


------------------------------------------------------------------------------
-- | Build a dictionary for AddAnnotation.
mkAddAnnDict :: TransitiveAnnsData -> TA.Annotation -> CoreExpr
mkAddAnnDict tad (TA.Annotation loc api method) = mkClassDict (tad_add_ann tad)
  [ Type $ mkTyConTy $ promoteDataCon $ enumToDataCon (tad_loc_tc tad) loc
  , Type $ mkStrLitTy $ fromString api
  , Type $ mkStrLitTy $ fromString method
  , Type typeAny
  ]


------------------------------------------------------------------------------
-- | Build a dictionary for KnownAnnotations.
mkKnownAnnsDict :: TransitiveAnnsData -> [TA.Annotation] -> CoreExpr
mkKnownAnnsDict tad z = mkClassDict (tad_knownanns tad)
  [ Type $ anyTypeOfKind liftedTypeKind
  , mkAnnotations tad z
  ]


------------------------------------------------------------------------------
-- | Build a core expression for a list of annotations.
mkAnnotations :: TransitiveAnnsData -> [TA.Annotation] -> Expr Var
mkAnnotations tad = mkListExpr (mkTyConTy $ tad_ann_tc tad) . fmap (mkAnnotation tad)


------------------------------------------------------------------------------
-- | Build a dictionary for ToHasAnnotations.
mkToHasAnnsDict :: TransitiveAnnsData ->  CoreExpr
mkToHasAnnsDict tad
  = mkClassDict (tad_to_has_ann tad)
  $ pure
  $ Type
  $ anyTypeOfKind liftedTypeKind

