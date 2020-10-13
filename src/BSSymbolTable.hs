module BSSymbolTable where

import qualified BSAST as AST
import Data.Map (Map)

newtype FunctionSig = FunctionSig ([AST.Param], AST.VarType)

newtype RecordSig = RecordSig (Map String AST.VarType)

newtype VariableSig = VariableSig (Maybe String, AST.VarType)

data SymTable = SymTable
  { functions :: Map String FunctionSig,
    records :: Map String RecordSig,
    variable :: Map String VariableSig
  }