{
module BSLexer 
  ( runLexer
  , Token(..)
  , AlexPosn(..)
) where
}
%wrapper "posn"

$digit   = 0-9 
$alpha   = [a-zA-Z] 
$alnum   = [ $alpha $digit ]
@ident   = $alpha [ $alnum \_ ]*
@string  = \" ([^ \" \t \n \\ ] | \\ . )* \"
@comment = \# .*
@number  = $digit+



rules :-
  $white+   ;
  if        { mkTok T_If }
  else      { mkTok T_Else }
  elseif    { mkTok T_ElseIf }
  type      { mkTok T_Type }
  fn        { mkTok T_Fn }
  int       { mkTok T_Int }
  bool      { mkTok T_Bool }
  string    { mkTok T_Str }
  true      { mkTok T_True }
  false     { mkTok T_False }
  let       { mkTok T_Let }
  for       { mkTok T_For }
  return    { mkTok T_Return }
  mod       { mkTok T_Module }
  ⊸         { mkTok T_LinearArrow }
  &&        { mkTok T_And }
  \|\|      { mkTok T_Or }
  \{        { mkTok T_LBrace }
  \}        { mkTok T_RBrace }
  \[        { mkTok T_LBracket }
  \]        { mkTok T_RBracket }
  \(        { mkTok T_LParen }
  \)        { mkTok T_RParen }
  \:        { mkTok T_Colon }
  \,        { mkTok T_Comma }
  \;        { mkTok T_Semi }
  \.        { mkTok T_Dot }
  \==       { mkTok T_Eq }
  \=        { mkTok T_Assign }
  \!\=      { mkTok T_Neq }
  \<        { mkTok T_Lesser }
  \<\=      { mkTok T_LesserEq }
  \>        { mkTok T_Greater }
  \>\=      { mkTok T_GreaterEq }
  \+        { mkTok T_Add }
  \->       { mkTok T_ReturnT }
  \-        { mkTok T_Sub }
  \*        { mkTok T_Mul }
  \/        { mkTok T_Div }
  @comment  ;
  @string   { (\p s -> T_String p ((tail . init ) s)) }
  @number   { (\p s -> T_Number p (read s) )}
  @ident    { T_Ident }



{



data Token
    = T_Type AlexPosn
    | T_RBracket AlexPosn
    | T_LBracket AlexPosn
    | T_And AlexPosn
    | T_Or AlexPosn
    | T_LBrace AlexPosn
    | T_RBrace AlexPosn
    | T_LParen AlexPosn
    | T_RParen AlexPosn
    | T_Colon AlexPosn
    | T_Comma AlexPosn
    | T_Semi AlexPosn
    | T_Dot AlexPosn
    | T_Eq  AlexPosn
    | T_Assign AlexPosn
    | T_Neq AlexPosn
    | T_Lesser AlexPosn
    | T_LesserEq AlexPosn
    | T_Greater AlexPosn
    | T_GreaterEq AlexPosn
    | T_Add AlexPosn
    | T_Sub AlexPosn
    | T_Mul AlexPosn
    | T_Div AlexPosn
    | T_String AlexPosn String
    | T_Number AlexPosn Integer
    | T_Ident AlexPosn String
    | T_If AlexPosn
    | T_ElseIf AlexPosn
    | T_Else AlexPosn
    | T_Fn AlexPosn
    | T_ReturnT AlexPosn
    | T_Int AlexPosn
    | T_Bool AlexPosn
    | T_Str AlexPosn
    | T_True AlexPosn
    | T_False AlexPosn
    | T_Let AlexPosn
    | T_For AlexPosn 
    | T_Return AlexPosn
    | T_LinearArrow AlexPosn
    | T_Module AlexPosn 
    deriving (Show, Eq)

mkTok :: (AlexPosn -> Token) -> AlexPosn -> String -> Token 
mkTok f p s = f p


runLexer :: String -> Either String [Token]
runLexer str = go (alexStartPos,'\n',[],str)
  where 
    go inp@(pos,_,_,s) = 
      case alexScan inp 0 of
        AlexEOF 
          -> Right []
        AlexError ((AlexPn _ l c),_,_,_) 
          -> let ls = lines str in
             let err = if c > 0 && l > 0 && l <= length ls
                       then ":\n" ++ (ls !! (l - 1)) ++ "\n" 
                            ++ replicate (c - 1) ' ' ++ "^ here"
                       else ""
             in Left $ "Lexical error at line " ++ show l 
                       ++ ", column " ++ (show c) ++ err
        AlexSkip inp' len     
          -> go inp' 
        AlexToken inp' len act 
          -> go inp' >>= Right . (:) (act pos $ take len s)

}