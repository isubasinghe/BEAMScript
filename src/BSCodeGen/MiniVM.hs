{-# LANGUAGE RecursiveDo #-}

module BSCodeGen.MiniVM where

import qualified BSAST as A
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified MiniVM.AST as VMA
import qualified MiniVM.IRBuilder as VM

data Env = Env
  { tySizes :: M.Map A.Ident Int,
    locs :: M.Map A.Ident VMA.Reg
  }
  deriving (Show, Eq)

type Codegen = VM.IRBuilderT (ST.State Env)

emitParams :: [A.Param] -> Codegen ()
emitParams [] = pure ()
emitParams ((A.Param pty pname) : ps) = do
  r <- VM.freshReg
  locs' <- ST.lift $ ST.gets locs
  ST.lift (ST.modify $ \s -> s {locs = M.insert pname r locs'})
  emitParams ps

emitExpr :: A.Expr -> Codegen VMA.Reg
emitExpr e = undefined

-- Just for tracking if / else etc

emitStatement :: A.Statement -> Codegen ()
emitStatement (A.If e ess eless eess) = mdo
  {- ebool <- emitExpr e
  emitCondr ebool thenBlock elseBlock
  thenBlock <- emitBranch "elif1" eess
  elseBlock <- emitBranch "elif1" eess -}
  undefined
emitStatement (A.For e s) = do
  loopHeader <- VM.buildLoopHeader
  ebool <- emitExpr e
  emitStatements s
  loopEnd <- VM.buildLoopHeaderEnd loopHeader
  undefined
emitStatement (A.Decl _ _) = undefined
emitStatement (A.AssignDecl _ _ _) = undefined
emitStatement (A.Assign _ _) = undefined
emitStatement (A.Call _ _) = undefined
emitStatement (A.ReturnExpr _) = undefined
emitStatement A.Return = undefined
emitStatement (A.Block _) = undefined

emitStatements :: [A.Statement] -> Codegen ()
emitStatements [] = pure ()
emitStatements (s : ss) = do
  emitStatement s
  emitStatements ss

emitFunction :: A.Function -> Codegen ()
emitFunction (A.Function fname ps r ss) = do
  VM.buildFunc fname $ do
    -- throw away reg0
    VM.freshReg
    emitParams ps
    emitStatements ss

-- getElementPtr

-- very simple, each type decl gets its own entry in the array
extractShallowTypDeclSzs :: A.TypeDecl -> Codegen Int
extractShallowTypDeclSzs (A.TypeDecl _ fields) = pure $ length fields

emitProgram :: String -> [A.TypeDecl] -> [A.Function] -> Codegen ()
emitProgram mname typs fns = do
  ssz <- mapM extractShallowTypDeclSzs typs
  let mappings = zip (map (\(A.TypeDecl name _) -> name) typs) ssz
  let myTySizes = M.fromList mappings
  ST.lift (ST.modify $ \s -> s {tySizes = myTySizes})
  undefined

codegenProgram :: A.Program -> VMA.Assembly
codegenProgram (A.Program fname mname ts fs) = undefined
codegenProgram (A.ProgramHappy mname ts fs) = codegenProgram (A.Program "unknown" mname ts fs)
