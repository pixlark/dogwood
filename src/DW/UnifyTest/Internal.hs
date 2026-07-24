{-# LANGUAGE PatternSynonyms #-}

module DW.UnifyTest.Internal where

import DW.Common hiding (Result)

import Data.Bifunctor (bimap, second)
import Data.Either (rights)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Traversable (for)

type Result a = Either Text a

newtype TyVarId = TyVarId Int
  deriving (Show, Eq)

instance Hashable TyVarId where
  hash (TyVarId id) = hash id
  hashWithSalt salt (TyVarId id) = hashWithSalt salt id

data Constraint = Constraint {typeclass :: Text, params :: [Ty]}
  deriving (Show, Eq)

data Ty = Void | Int | Bool | CTy Constraint | TyVar TyVarId
  deriving (Show, Eq)

newtype Subst = Subst (HashMap TyVarId Ty)
  deriving (Show, Eq)

mkTyVar :: Int -> Ty
mkTyVar id = TyVar (TyVarId id)

mkSubst :: Subst
mkSubst = Subst HashMap.empty

subst :: (TyVarId, Ty) -> Ty -> Ty
subst (id, forType) (TyVar id') | id == id' = forType
subst (id, forType) (CTy c@(Constraint {params})) = CTy (c {params = subst (id, forType) <$> params})
subst _ t = t

substFromList :: [(TyVarId, Ty)] -> Subst
substFromList = Subst . HashMap.fromList

mapValues :: (Hashable k) => (a -> b) -> HashMap k a -> HashMap k b
mapValues f = HashMap.fromList . map (second f) . HashMap.toList

addSubst :: (TyVarId, Ty) -> Subst -> Result Subst
addSubst (id, forType) (Subst s) = case HashMap.lookup id s of
  Just existing -> Left (typeMismatch existing forType)
  Nothing -> Right $ Subst $ HashMap.insert id forType $ subst (id, forType) `mapValues` s

typeMismatch :: Ty -> Ty -> Text
typeMismatch a b = toStrict $ format "type mismatch: {} and {}" (Shown a, Shown b)

incompatibleClasses :: Text -> Text -> Text
incompatibleClasses a b = toStrict $ format "incompatible typeclasses: {} and {}" (a, b)

wrongParamCount :: Int -> Int -> Text
wrongParamCount a b = toStrict $ format "incompatible param counts: {} and {}" (a, b)

modifyFallible :: (Show e, State a :> es, Error e :> es) => (a -> Either e a) -> Eff es ()
modifyFallible f = do
  a <- get
  case f a of
    Left e -> throwError e
    Right x -> put x

occurs :: TyVarId -> Ty -> Bool
occurs t1 (TyVar t2) = t1 == t2
occurs t (CTy (Constraint {params})) = any (occurs t) params
occurs _ _ = False

unifyVar :: (State Subst :> es, Error Text :> es) => TyVarId -> Ty -> Eff es ()
-- The constraint is just T ~ T, which is trivially true and changes nothing
unifyVar id (TyVar id') | id == id' = return ()
-- Otherwise, perform the occurs check and then substitute
unifyVar id ty =
  if occurs id ty
    then throwError "recursive type"
    else modifyFallible $ addSubst (id, ty)

applySubst :: Subst -> Ty -> Ty
applySubst (Subst s) t = foldl' (flip subst) t (HashMap.toList s)

performUnify :: (State Subst :> es, Error Text :> es) => Ty -> Ty -> Eff es ()
performUnify Void Void = return ()
performUnify Int Int = return ()
performUnify Bool Bool = return ()
performUnify (TyVar id) ty = do s <- get; unifyVar id (applySubst s ty)
performUnify ty (TyVar id) = do s <- get; unifyVar id (applySubst s ty)
performUnify (CTy (Constraint {typeclass = t1})) (CTy (Constraint {typeclass = t2})) | t1 /= t2 = throwError $ incompatibleClasses t1 t2
performUnify (CTy (Constraint {params = p1})) (CTy (Constraint {params = p2}))
  | length p1 /= length p2 = throwError $ wrongParamCount (length p1) (length p2)
  | otherwise = do
      forM_ (p1 `zip` p2) $ \(a, b) -> do
        s <- get
        performUnify (applySubst s a) (applySubst s b)
performUnify a b = throwError $ typeMismatch a b

unify :: Ty -> Ty -> Result Subst
unify a b = runPureEff $ runErrorNoCallStack $ execState mkSubst $ performUnify a b

type ClassName = Text
type MethodName = Text

newtype Method = Method MethodName
  deriving (Show)

data Typeclass = Typeclass ClassName [TyVarId] [Method]
  deriving (Show)

data Instance = Instance ClassName [Ty]
  deriving (Show, Eq)

data Call = Call {clsName :: ClassName, methodName :: MethodName, typeArgs :: [Ty], constraints :: [Constraint]}
  deriving (Show)

data Program = Program [Typeclass] [Instance]
  deriving (Show)

resolveClass :: ClassName -> MethodName -> [Typeclass] -> Result Typeclass
resolveClass className methodName classes = case filter (\(Typeclass n _ _) -> n == className) classes of
  [cls@(Typeclass _ _ ms)] -> case filter (\(Method n) -> n == methodName) ms of
    [_] -> return cls
    _ -> Left $ "no such method " `Text.append` methodName
  _ -> Left $ "no such typeclass " `Text.append` className

isPolymorphic :: Ty -> Bool
isPolymorphic (TyVar _) = True
isPolymorphic (CTy (Constraint {params})) = any isPolymorphic params
isPolymorphic _ = False

data Resolution = ResolvePolymorphic | ResolveConcrete Instance
  deriving (Show, Eq)

resolveInstance :: Call -> Program -> Result Resolution
resolveInstance (Call clsName methodName args localConstraints) (Program classes instances) = do
  (Typeclass _ params _) <- resolveClass clsName methodName classes
  when (length args /= length params) $ Left "wrong number of type arguments"
  let constraint = Constraint clsName args
  if isPolymorphic (CTy constraint)
    then do
      let matching = flip filter localConstraints $ \localConstraint ->
            isRight $ CTy constraint `unify` CTy localConstraint
      case matching of
        [] -> Left "no matching constraint in context"
        _ -> return ResolvePolymorphic
    else do
      let matching = flip filter instances $ \(Instance instName instArgs) ->
            let instConstraint = Constraint instName instArgs
             in isRight $ CTy constraint `unify` CTy instConstraint
      case matching of
        [inst] -> return $ ResolveConcrete inst
        [] -> Left "no matching instance"
        _ -> Left "ambiguous - multiple matching instances"
