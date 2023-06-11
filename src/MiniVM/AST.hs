module MiniVM.AST where

newtype Reg = Reg String
  deriving (Show, Eq)

newtype FuncName = FuncName String
  deriving (Show, Eq)

newtype LabelName = LabelName String
  deriving (Show, Eq)

newtype Assembly = Assembly [Statement]
  deriving (Show, Eq)

data Instr
  = OpExit
  | OpReg Reg Reg
  | OpFunc FuncName [String]
  | OpJump LabelName
  | OpCall Reg LabelName [Reg]
  | OpDCall Reg Reg [Reg]
  | OpRet Reg
  | OpInt Reg Int
  | OpAdd Reg Reg Reg
  | OpSub Reg Reg Reg
  | OpMul Reg Reg Reg
  | OpDiv Reg Reg Reg
  | OpMod Reg Reg Reg
  | OpPBB Reg LabelName LabelName
  | OpBeq Reg Reg LabelName LabelName
  | OpBlt Reg Reg LabelName LabelName
  | OpStr String 
  | OpArr Reg Reg 
  | OpSet Reg Reg Reg 
  | OpGet Reg Reg Reg 
  | OpLen Reg Reg 
  | OpType Reg Reg
  deriving (Show, Eq)

data Statement
  = Label LabelName
  | Instr
  deriving (Show, Eq)
