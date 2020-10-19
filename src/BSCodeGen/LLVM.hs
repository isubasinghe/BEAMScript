module BSCodeGen.LLVM where

import qualified BSAST as BSAST
import Data.String
import LLVM.AST
import LLVM.Pretty (ppllvm)

getType :: BSAST.VarType -> Type
getType (BSAST.VBool) = IntegerType {typeBits = 8} -- one byte for booleans
getType (BSAST.VInt) = IntegerType {typeBits = 64} -- only support 64 bit
getType (BSAST.VType s) = NamedTypeReference (mkName s)

getTypes :: [BSAST.VarType] -> [Type]
getTypes [] = []
getTypes (t : ts) = (getType t) : (getTypes ts)

compileStruct :: BSAST.TypeDecl -> Definition
compileStruct (BSAST.TypeDecl n t) = TypeDefinition (mkName n) (Just StructureType {isPacked = False, elementTypes = types})
  where
    types = getTypes (map snd t)

compile :: BSAST.Program -> Module
compile (BSAST.Program m f ss fs) =
  Module
    { moduleName = fromString m,
      moduleSourceFileName = fromString f,
      moduleDataLayout = Nothing,
      moduleTargetTriple = Nothing,
      moduleDefinitions = structs
    }
  where
    structs = map compileStruct ss
