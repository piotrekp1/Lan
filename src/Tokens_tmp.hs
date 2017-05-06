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
   bind                       { \s -> TokenBind }
   Int                        { \s -> TokenType s }
   Bool                       { \s -> TokenType s }
   True                       { \s -> TokenBool True }
   False                      { \s -> TokenBool False }
   and                        { \s -> TokenAnd }
   or                         { \s -> TokenOr }
   $alpha [$alpha $digit \_ \']* { \s -> TokenVar s }
   "\"                         { \s -> TokenBackslash }
   $digit+                    { \s -> TokenInt (read s)}
   \:\=                        { \s -> TokenDfn }
   \-\>                        { \s -> TokenArrow }
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
   \<                          { \s -> TokenLT }
   \>                          { \s -> TokenGT }
   \:\:                          { \s -> TokenDecl }
   \:                          { \s -> TokenTwoDots }


{
data Token
      = TokenLet
      | TokenIn
      | TokenInt Int
      | TokenBool Bool
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
      deriving Show

lanTokens = alexScanTokens

}
