{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha =[a-zA-Z]

tokens :-
   $white+                    ;
   "//".*                     ;
   while                      { \s -> TokenWhile }
   if                         { \s -> TokenIf }
   then                       { \s -> TokenThen }
   else                       { \s -> TokenElse }
   let                        { \s -> TokenLet }
   in                         { \s -> TokenIn }
   def                        { \s -> TokenDef }
   bind                       { \s -> TokenBind }
   Int                        { \s -> TokenType s }
   Bool                       { \s -> TokenType s }
   Char                       { \s -> TokenType s }
   True                       { \s -> TokenBool True }
   False                      { \s -> TokenBool False }
   print                      { \s -> TokenPreDefFoo s }
   length                     { \s -> TokenPreDefFoo s }
   show                       { \s -> TokenPreDefFoo s }
   and                        { \s -> TokenAnd }
   or                         { \s -> TokenOr }
   $alpha [$alpha $digit \_ \']* { \s -> TokenVar s }
   "\"                         { \s -> TokenBackslash }
   $digit+                    { \s -> TokenInt (read s)}
   '.'                         { \s -> TokenChar (read s)}
   \"[^\"]*\"                  { \s -> TokenString (read s) }
   \:\=                        { \s -> TokenDfn }
   \+\=                         { \s -> TokenMod "Add" }
   \-\=                         { \s -> TokenMod "Sub" }
   \*\=                        { \s ->  TokenMod "Mul" }
   \/\=                       { \s ->  TokenMod "Div"  }
   \+\+                        { \s -> TokenModInPlace "Add" }
   \-\-                         { \s -> TokenModInPlace "Sub"}
   \-\>                        { \s -> TokenArrow }
   \!\=                         { \s -> TokenNotEq }
   \=\=                         { \s -> TokenCmp }
   \=                          { \s -> TokenEq }
   \+                          { \s -> TokenPlus }
   \-                          { \s -> TokenMinus }
   \*                          { \s -> TokenTimes }
   \/                          { \s -> TokenDiv }
   \(                          { \s -> TokenOB }
   \)                          { \s -> TokenCB }
   \;                          { \s -> TokenSep }
   \{                          { \s -> TokenLBracket }
   \}                          { \s -> TokenRBracket }
   \[\:                        { \s -> TokenArrDefOB }
   \:\]                        { \s -> TokenArrDefCB }
   \[                          { \s -> TokenArrayOB }
   \]                          { \s -> TokenArrayCB }
   \<                          { \s -> TokenLT }
   \>                          { \s -> TokenGT }
   \:\:                          { \s -> TokenDecl }
   \:                          { \s -> TokenTwoDots }
   ","                           { \s -> TokenComma }
   "_""_"*                      { \s -> TokenDeclSep }


{
data Token
      = TokenLet
      | TokenIn
      | TokenInt Int
      | TokenBool Bool
      | TokenChar Char
      | TokenString String
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
      | TokenSep
      | TokenIf
      | TokenThen
      | TokenElse
      | TokenRBracket
      | TokenLBracket
      | TokenWhile
      | TokenType String
      | TokenDecl
      | TokenAnd
      | TokenOr
      | TokenCmp
      | TokenGT
      | TokenLT
      | TokenTrue
      | TokenFalse
      | TokenTwoDots
      | TokenArrow
      | TokenDfn
      | TokenBind
      | TokenBackslash
      | TokenDeclSep
      | TokenArrayOB
      | TokenArrayCB
      | TokenArrDefOB
      | TokenArrDefCB
      | TokenComma
      | TokenMinusMinus
      | TokenModInPlace String
      | TokenMod String
      | TokenNotEq
      | TokenPreDefFoo String
      | TokenDef
      deriving Show

lanTokens = alexScanTokens

}
