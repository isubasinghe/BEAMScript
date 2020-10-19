{
module BSParser (
  runParser
  ,parse
  ,parseFile
) where

import BSLexer
import BSAST

}

%name runParser
%monad { Either String } { >>= } { return }
%tokentype { Token }
%error { parseError }

%token
    if      { T_If _ }
    else    { T_Else _ }
    elseif  { T_ElseIf _ }
    type    { T_Type _ }
    fn      { T_Fn _ }
    int     { T_Int _ }
    bool    { T_Bool _ }
    string  { T_Str _ }
    true    { T_True _ }
    false   { T_False _ }
    let     { T_Let _ }
    for     { T_For _ }
    return  { T_Return _ }
    mod     { T_Module _ }
    '&&'    { T_And _ }
    '->'    { T_ReturnT _ }
    ':'     { T_Colon _ }
    '('     { T_LParen _ }
    ')'     { T_RParen _ }
    '{'     { T_LBrace _ }
    '}'     { T_RBrace _ }
    ','     { T_Comma _ }
    '='     { T_Assign _ }
    ';'     { T_Semi _ }
    ident   { T_Ident _ $$ }
    string_ { T_String _ $$ }
    number  { T_Number _ $$ }    

%%

Program
  : ModuleName TypeDecls Functions { ProgramHappy $1 $2 $3 }

ModuleName
  : mod string_ ';' { $2 }

TypeDecls
  : {- empty -} { [] }
  | TypeDecls typedecl { $2:$1 }

typedecl :: { TypeDecl }
typedecl
  : type ident '{' fields '}' { TypeDecl $2 $4 }

fields
  : fields_ field  { $2:$1 }

fields_
  : {- empty -} { [] }
  | fields_ field { $2:$1 }

field :: { (Ident, VarType) }
field
  : ident ':' VarType ';' { ($1, $3) }

Functions
  : {- empty -} { [] }
  | Functions function { $2:$1 }

function
  : fn ident '(' Params ')' '{' Statements '}' { Function $2 $4 Void $7 }
  | fn ident '(' Params ')' '->' VarType '{' Statements '}' { Function $2 $4 $7 $9 }               

Params
  : {- empty -} { [] }
  | params      { $1 }

params
  : param { [$1] }
  | param ',' params { $1:$3 }

       
param 
  : ident ':' VarType { Param $3 $1}

Statements
  : {- empty -} { [] }
  | statement Statements { $1:$2 }

statement
  : let ident ':' VarType ';' { Decl $2 $4 }
  | let ident ':' VarType '=' Expr ';' { AssignDecl $2 $4 $6 }
  | Expr '=' Expr ';' { Assign $1 $3 }
  | if Expr '{' Statements '}' elseifs else '{' Statements '}' { If $2 $4 $6 $9 }
  | if Expr '{' Statements '}' elseifs { If $2 $4 $6 [] }
  | for Expr '{' Statements '}' { For $2 $4 }
  | '{' Statements '}' { Block $2 }
  | return Expr ';' { ReturnExpr $2 }
  | return ';' { Return }
  

elseifs :: { [(Expr, [Statement])] }
elseifs
  : {- empty -} { [] }
  | elseif_ elseifs { $1:$2 }

elseif_
  : elseif Expr '{' Statements '}' { ($2,$4) }

Expr
  : Constant { Constant $1 }
  | ident { LId $1 }
  | '(' Expr ')' { $2 }
  | '{' exprFields '}' { Record $2 }

exprFields
  : exprField { [$1] }
  | exprField ',' exprFields_ { $1:$3 }

exprFields_
  : {- empty -} { [] }
  | exprField ',' exprFields_ { $1:$3 }

exprField
  : ident ':' Expr { ($1,$3) }

Constant
  : string_ { CString $1 }
  | number { CInt $1 }
  | true { CBool True }
  | false { CBool False }

VarType
  : int { VInt }
  | ident { VType $1 }
  | string { VString }
  | bool { VBool }


{
transformAST :: String -> Program -> Program
transformAST f (ProgramHappy m ts fs) = Program f m ts fs


parseFile :: String -> String -> Either String Program
parseFile f s = (transformAST f) <$> (parse s)

parse :: String -> Either String Program
parse s = (runLexer s) >>= runParser

parseError :: [Token] -> Either String a
parseError [] = Left "Unxpected parse error at end of file"
parseError (x:_) = Left (show x)
}