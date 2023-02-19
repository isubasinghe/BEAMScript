module BSCodeGen.LLVM where

import qualified BSAST
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy.IO as TLIO
import Debug.Trace
import LLVM.AST
  ( Definition (GlobalDefinition, TypeDefinition),
    Global (Function),
    Module (..),
    Parameter (..),
    Type
      ( IntegerType,
        NamedTypeReference,
        StructureType,
        VoidType,
        elementTypes,
        isPacked,
        typeBits
      ),
    mkName,
  )
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import LLVM.Pretty (ppllvm)
import System.Directory

getType :: BSAST.VarType -> Type
getType (BSAST.VBool) = IntegerType {typeBits = 8} -- one byte for booleans
getType (BSAST.VInt) = IntegerType {typeBits = 64} -- only support 64 bit
getType (BSAST.VType s) = NamedTypeReference (mkName s)
getType (BSAST.Void) = VoidType

compileStruct :: BSAST.TypeDecl -> Definition
compileStruct (BSAST.TypeDecl n t) = TypeDefinition (mkName n) (Just StructureType {isPacked = False, elementTypes = types})
  where
    types = (map (getType . snd) t)

getParam :: BSAST.Param -> Parameter
getParam (BSAST.Param v n) = traceShow (Parameter (getType v) (mkName n) []) (Parameter (getType v) (mkName n) [])
{-# INLINE getParam #-}

compileFunction :: BSAST.Function -> Definition
compileFunction (BSAST.Function n ps r ss) =
  GlobalDefinition
    ( Function
        { G.linkage = L.External,
          G.visibility = V.Default,
          G.dllStorageClass = Nothing,
          G.callingConvention = CC.C,
          G.returnAttributes = [],
          G.returnType = getType r,
          G.name = mkName n,
          G.parameters = (map getParam ps, False),
          G.functionAttributes = [],
          G.section = Nothing,
          G.comdat = Nothing,
          G.alignment = 0,
          G.garbageCollectorName = Nothing,
          G.prefix = Nothing,
          G.basicBlocks = [],
          G.personalityFunction = Nothing,
          G.metadata = []
        }
    )

compileModule :: BSAST.Program -> Module
compileModule (BSAST.Program f m ss fs) =
  Module
    { moduleName = fromString m,
      moduleSourceFileName = fromString f,
      moduleDataLayout = Nothing,
      moduleTargetTriple = Nothing,
      moduleDefinitions = structs ++ functinos
    }
  where
    structs = map compileStruct ss
    functinos = map compileFunction fs

compilePretty :: Module -> String -> IO ()
compilePretty m f = TLIO.writeFile f (ppllvm m)

