module BSTyCheck where
import BSAST
import Control.Monad.State
import Control.Monad.Except ( ExceptT, MonadError(throwError) )


data TypeCtx = TypeCtx {}

data TyError = TyError String

type TyCtx = ExceptT TyError (State TypeCtx)


liftAssert :: (Eq a, Show a) => a -> a -> TyCtx ()
liftAssert a b = if a == b then pure () else throwError $ TyError (show a ++ "!=" ++ show b)

checkStatement :: Statement -> TyCtx ()
checkStatement (If cond rss lss as) = do
    condty <- checkExpr cond
    liftAssert condty VBool
    es <- mapM (\(e, _) -> checkExpr e) lss
    mapM_ (`liftAssert` VBool) es
checkStatement (For cond ss) = do 
    condty <- checkExpr cond
    liftAssert condty VBool 

checkStatement (Decl ident varty) = undefined
checkStatement (AssignRecord r vs) = undefined
checkStatement (AssignDecl id ty e) = undefined
checkStatement (Assign lhs rhs) = undefined
checkStatement (Call ident args) = undefined
checkStatement (ReturnExpr e) = undefined
checkStatement Return = undefined
checkStatement (Block ss) = undefined


checkExpr :: Expr -> TyCtx VarType
checkExpr = undefined