module BSTyCheck where
import BSAST
import Control.Monad.State
import Control.Monad.Except ( ExceptT, MonadError(throwError) )
import Data.Map (Map)


data TypeCtx = TypeCtx { fns :: Map String ([VarType], VarType) }

data TyError = TyError String

type TyCtx = ExceptT TyError (State TypeCtx)


liftAssert :: (Eq a, Show a) => a -> a -> TyCtx ()
liftAssert a b = if a == b then pure () else throwError $ TyError (show a ++ " != " ++ show b)


liftAssertDistinct :: [Maybe VarType] -> TyCtx (Maybe VarType)
liftAssertDistinct [] = pure Nothing
liftAssertDistinct [a] = pure a
liftAssertDistinct (a:b:as) = do 
  if a == b then 
    liftAssertDistinct (b:as)
  else 
    throwError $ TyError (show a ++ " != " ++ show b) 


checkStatement :: Statement -> TyCtx (Maybe VarType)
checkStatement (If cond rss lss as) = do
    condty <- checkExpr cond
    liftAssert condty VBool
    es <- mapM (\(e, _) -> checkExpr e) lss
    mapM_ (`liftAssert` VBool) es
    rstys <- mapM checkStatement rss
    rstys' <- liftAssertDistinct rstys
    lsstys <-  mapM (\(e, ss) -> mapM checkStatement ss) lss
    lsstys' <- mapM liftAssertDistinct lsstys
    lsstys'' <- liftAssertDistinct lsstys'
    astys <- mapM checkStatement as
    astys' <- liftAssertDistinct astys
    liftAssertDistinct [rstys', lsstys'', astys']
checkStatement (For cond ss) = do 
    condty <- checkExpr cond
    liftAssert condty VBool 
    pure Nothing 

checkStatement (Decl ident varty) = pure Nothing
checkStatement (AssignRecord r vs) = pure Nothing
checkStatement (AssignDecl id ty e) = pure Nothing
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
