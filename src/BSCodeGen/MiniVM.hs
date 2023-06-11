module BSCodeGen.MiniVM where
import qualified MiniVM.AST as VMA
import qualified MiniVM.IRBuilder as VM
import qualified Control.Monad.State as ST
import qualified BSAST as A
import qualified Data.Map as M


data Env = Env 
    { arrSizes :: M.Map A.Ident Int 
    , locs :: M.Map A.Ident VMA.Reg
    }
  deriving (Show, Eq)

type Codegen = VM.IRBuilderT (ST.State Env)

emitParams :: [A.Param] -> Codegen ()
emitParams = undefined 

blah fn = fn

emitFunction :: A.Function -> Codegen ()
emitFunction (A.Function fname ps r ss) = do 
  VM.buildFunc fname $ do 
    _ <- VM.freshReg 
    undefined
   
codegenProgram :: A.Program -> VMA.Assembly
codegenProgram (A.Program fname mname ts fs) = undefined
codegenProgram (A.ProgramHappy mname ts fs) = codegenProgram (A.Program "unknown" mname ts fs)
