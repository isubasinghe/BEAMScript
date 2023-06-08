module BSTyCheck where

import BSAST
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State
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
  put $ s{vars=vars'}
  pure Nothing

checkStatement (AssignDecl id ty e) = do 
  undefined
checkStatement (Assign lhs rhs) = do
  lhsty <- checkExpr lhs
  rhsty <- checkExpr rhs
  liftAssert lhsty rhsty
  pure Nothing
checkStatement (Call ident args) = do
  argsty <- mapM checkExpr args
  x <- get
  undefined
checkStatement (ReturnExpr e) = undefined
checkStatement Return = undefined
checkStatement (Block ss) = undefined

checkExpr :: Expr -> TyCtx VarType
checkExpr = undefined

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
