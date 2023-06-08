{-# LANGUAGE StrictData #-}
module BSAST where

type FileName = String

type Ident = String

type ModuleName = String

data Source = Source [Program]
  deriving (Show)

data TypeDecl = TypeDecl !Ident ![(Ident, VarType)]
  deriving (Show)

data Program
  = Program !FileName !ModuleName ![TypeDecl] ![Function]
  | ProgramHappy !ModuleName ![TypeDecl] ![Function]
  deriving (Show)

data VarType
  = VInt
  | VType !Ident
  | VDouble
  | VString
  | VBool
  | Void
  deriving (Show, Eq)

data Function = Function !Ident ![Param] !VarType ![Statement]
  deriving (Show)

data Param = Param !VarType !Ident
  deriving (Show)

data Expr
  = Constant !Constant
  | LId !Ident
  | Record ![(Ident, Expr)]
  | CallRVal !Ident ![Expr]
  deriving (Show)

data Statement
  = If !Expr ![Statement] ![(Expr, [Statement])] ![Statement]
  | For !Expr ![Statement]
  | Decl !Ident !VarType
  | AssignDecl !Ident !VarType !Expr
  | Assign !Expr !Expr
  | Call !Ident ![Expr]
  | ReturnExpr !Expr
  | Return
  | Block ![Statement]
  deriving (Show)

data Constant
  = CInt !Integer
  | CDouble !Double
  | CString !String
  | CBool !Bool
  deriving (Show)
