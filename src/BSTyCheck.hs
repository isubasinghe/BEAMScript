{-# LANGUAGE TupleSections #-}

module BSTyCheck where

import BSAST
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State
import Data.Equality.Graph.Lens (_data)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

data TypeCtx = TypeCtx
  { fns :: Map Ident ([VarType], VarType),
    vars :: Map Ident VarType,
    rectys :: Map Ident (Map Ident VarType)
  }
  deriving (Show)

defaultCtx :: TypeCtx
defaultCtx = TypeCtx {fns = M.empty, vars = M.empty, rectys = M.empty}

invertBijection :: (Ord k, Ord v) => M.Map k v -> M.Map v k
invertBijection = M.foldrWithKey (flip M.insert) M.empty

newtype TyError = TyError String deriving (Show)

type TyCtx = ExceptT TyError (State TypeCtx)

liftAssert :: (Eq a, Show a) => a -> a -> TyCtx ()
liftAssert a b = if a == b then pure () else throwError $ TyError (show a ++ " != " ++ show b)

liftAssertDistinct :: [Maybe VarType] -> TyCtx (Maybe VarType)
liftAssertDistinct [] = pure Nothing
liftAssertDistinct [a] = pure a
liftAssertDistinct (a : b : as) = do
  if a == b
    then liftAssertDistinct (b : as)
    else throwError $ TyError (show a ++ " != " ++ show b)

liftAssertFnExists :: Ident -> TyCtx ()
liftAssertFnExists fname = do
  s <- get
  if M.member fname (fns s)
    then pure ()
    else throwError $ TyError (fname ++ " does not exist")

liftAssertValidType :: VarType -> TyCtx ()
liftAssertValidType (VType ident) = do
  s <- get
  let vrecs = rectys s
  let maybeFields = M.lookup ident vrecs
  case maybeFields of
    Just _ -> do
      pure ()
    Nothing -> throwError $ TyError (ident ++ " is an invalid type")
liftAssertValidType _ = pure ()

liftSameSz :: (Foldable t, Foldable v) => t a -> v b -> String -> TyCtx ()
liftSameSz ts vs msg =
  if length ts /= length vs
    then throwError $ TyError msg
    else pure ()

liftVarExists :: Ident -> VarType -> TyCtx ()
liftVarExists ident vty = do
  s <- get
  let vars' = vars s
  let maybeVar = M.lookup ident vars'
  case maybeVar of
    Just ity -> liftAssert vty ity
    Nothing -> throwError $ TyError (ident ++ "is an invalid variable")

liftInferAssertRecord :: VarType -> [(Ident, Expr)] -> TyCtx ()
liftInferAssertRecord (VType name) rs = do
  s <- get
  let existingRecords = rectys s
  let maybeRecord = M.lookup name existingRecords
  case maybeRecord of
    Just mappings -> checkMappings mappings S.empty rs
    Nothing -> throwError $ TyError (name ++ " is not a valid record")
  where
    checkMappings :: M.Map Ident VarType -> S.Set Ident -> [(Ident, Expr)] -> TyCtx ()
    checkMappings ms s [] = do
      let msSet = M.keysSet ms
      let rsSet = S.fromList (map fst rs)
      let missingFields = S.difference msSet rsSet
      if not (null missingFields)
        then throwError $ TyError (show missingFields ++ " are missing in decl of " ++ name)
        else pure ()
    checkMappings ms s ((rname, rexpr) : rs) = do
      case M.lookup rname ms of
        Just rty -> do
          checkAssertExpr rty rexpr
          checkMappings ms (S.insert rname s) rs
        Nothing -> throwError $ TyError (rname ++ " does not exist in field for " ++ name)
      pure ()
liftInferAssertRecord _ rs = pure ()

liftAssertFnCall :: VarType -> Ident -> [Expr] -> TyCtx ()
liftAssertFnCall v fname es = do
  s <- get
  let fns' = fns s
  case M.lookup fname fns' of
    Just (argtys, retty) -> do
      mapM_ (uncurry checkAssertExpr) (zip argtys es) -- type check params
      liftAssert retty v -- type check return type
    Nothing -> throwError $ TyError ("couldn't find function " ++ fname)

getType :: Constant -> VarType
getType (CInt _) = VInt
getType (CDouble _) = VDouble
getType (CString _) = VString
getType (CBool _) = VBool

checkAssertExpr :: VarType -> Expr -> TyCtx ()
checkAssertExpr v e = case e of
  (Constant c) -> liftAssert v $ getType c
  (LId id) -> do
    liftVarExists id v
  (Record rs) -> do
    liftInferAssertRecord v rs
  (CallRVal fname ps) -> liftAssertFnCall v fname ps
  (BinOp op lhs rhs) -> do
    -- DANGEROUS -> can lead to infinite recursion
    ty <- checkExpr e
    liftAssert v ty

checkStatement :: Statement -> TyCtx (Maybe VarType)
checkStatement (If cond rss lss as) = do
  condty <- checkExpr cond
  liftAssert condty VBool
  es <- mapM (\(e, _) -> checkExpr e) lss
  mapM_ (`liftAssert` VBool) es
  pure Nothing
checkStatement (For cond ss) = do
  condty <- checkExpr cond
  liftAssert condty VBool
  pure Nothing
checkStatement (Decl ident varty) = do
  s <- get
  liftAssertValidType varty
  let vars' = M.insert ident varty (vars s)
  put $ s {vars = vars'}
  pure Nothing
checkStatement (AssignDecl id ty e) = do
  checkAssertExpr ty e
  s <- get
  let vars' = vars s
  put $ s {vars = M.insert id ty vars'}
  pure Nothing
checkStatement (Assign lhs rhs) = do
  lhsty <- checkExpr lhs
  rhsty <- checkExpr rhs
  liftAssert lhsty rhsty
  pure Nothing
checkStatement (Call ident args) = do
  s <- get
  let fns' = fns s
  case M.lookup ident fns' of
    Just (argsty, retty) -> do
      liftAssertFnCall retty ident args
      pure $ Just retty
    Nothing -> throwError $ TyError (ident ++ " is not a valid function")
checkStatement (ReturnExpr e) = Just <$> checkExpr e
checkStatement Return = pure Nothing
checkStatement (Block ss) = error "Block statements are not supported yet"

-- structurally match for Record types (ewww)
checkExpr :: Expr -> TyCtx VarType
checkExpr (Constant c) = pure $ getType c
checkExpr (LId ident) = do
  s <- get
  let vars' = vars s
  case M.lookup ident vars' of
    Just ty -> pure ty
    Nothing -> throwError $ TyError (ident ++ " does not exist")
checkExpr (Record rs) = do
  s <- get
  let invertedRectyls = invertBijection (rectys s)
  rstys <- mapM (\(fst, snd) -> (fst,) <$> checkExpr snd) rs
  let rstysMaps = M.fromList rstys
  case M.lookup rstysMaps invertedRectyls of
    Just ident -> pure $ VType ident
    Nothing -> throwError $ TyError " was unable find structurally find type"
checkExpr (CallRVal fname ps) = do
  s <- get
  let fns' = fns s
  case M.lookup fname fns' of
    Just (argsty, retty) -> do
      mapM_ (uncurry checkAssertExpr) (zip argsty ps) -- type check params
      pure retty
    Nothing -> throwError $ TyError (fname ++ " does not exist")
checkExpr (BinOp op lhs rhs) = do
  lhsty <- checkExpr lhs
  rhsty <- checkExpr rhs
  if lhsty /= rhsty
    then throwError $ TyError (show lhs ++ " != " ++ show rhs)
    else case (lhsty, op) of
      (VInt, BAdd) -> pure VInt
      (VInt, BMinus) -> pure VInt
      (VInt, BMul) -> pure VInt
      (VInt, BDiv) -> pure VInt
      (_, BAdd) -> undefinedBinOP op lhsty
      (_, BMinus) -> undefinedBinOP op lhsty
      (_, BMul) -> undefinedBinOP op lhsty
      (_, BDiv) -> undefinedBinOP op lhsty
      (VInt, BLe) -> pure VBool
      (VInt, BLeq) -> pure VBool
      (VInt, BGe) -> pure VBool
      (VInt, BGeq) -> pure VBool
      (VInt, BAnd) -> pure VBool
      (VInt, BEq) -> pure VBool
      (VInt, BNeq) -> pure VBool
      (_, BLe) -> undefinedBinOP op lhsty
      (_, BLeq) -> undefinedBinOP op lhsty
      (_, BGe) -> undefinedBinOP op lhsty
      (_, BGeq) -> undefinedBinOP op lhsty
      (_, BAnd) -> undefinedBinOP op lhsty
      (_, BEq) -> undefinedBinOP op lhsty
      (_, BNeq) -> undefinedBinOP op lhsty
  where
    undefinedBinOP :: Op -> VarType -> TyCtx VarType
    undefinedBinOP op vty = throwError $ TyError (show op ++ " is not defined for " ++ show vty)

collectIdents :: [VarType] -> [Ident]
collectIdents [] = []
collectIdents (v : vs) =
  case v of
    (VType ident) -> ident : collectIdents vs
    _ -> collectIdents vs

checkFunction :: Function -> TyCtx ()
checkFunction (Function name params retsort statements) = do
  liftAssertFnExists name
  let ptypes = map (\(Param vty _) -> vty) params
  mapM_ liftAssertValidType ptypes
  liftAssertValidType retsort
  mapM_ checkStatement statements
  s <- get
  let vars' = M.empty
  put $ s {vars = vars'}

checkRecord :: TypeDecl -> TyCtx ()
checkRecord (TypeDecl tname mappings) = do
  let recordNames = map snd mappings
  mapM_ liftAssertValidType recordNames

checkProgram :: Program -> TyCtx ()
checkProgram (Program fname mname recs fs) = checkProgram (ProgramHappy mname recs fs)
checkProgram (ProgramHappy mname recs fs) = do
  let typdecls = M.fromList $ map (\(TypeDecl tname rcs) -> (tname, M.fromList rcs)) recs
  liftSameSz typdecls recs "There was a duplicate record declared"
  let fnames = map (\(Function fname ps r _) -> (fname, (map (\(Param v _) -> v) ps, r))) fs
  let fnmap = M.fromList fnames
  liftSameSz fnmap fs "There was a duplicate function declared"
  put $ TypeCtx {fns = fnmap, vars = M.empty, rectys = typdecls}
  mapM_ checkRecord recs
  mapM_ checkFunction fs

getProgState :: Program -> (Either TyError (), TypeCtx)
getProgState p = runState (runExceptT (checkProgram p)) defaultCtx
