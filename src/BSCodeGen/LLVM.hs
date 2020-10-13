module BSCodeGen.LLVM where

import qualified BSAST as BSAST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float
import LLVM.AST.FloatingPointPredicate hiding (False, True)
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Global
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Operand
import LLVM.AST.Type as Type
import LLVM.IRBuilder

compileStruct :: [BSAST.TypeDecl] -> a2
compileStruct ((BSAST.TypeDecl _ t) : ss) = undefined

compile :: BSAST.Program -> _a
compile (BSAST.Program f ss fs) = undefined