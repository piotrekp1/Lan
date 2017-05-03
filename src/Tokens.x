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
   Int                        { \s -> TokenType s }
   True                       { \s -> TokenTrue }
   False                      { \s -> TokenFalse }
   and                        { \s -> TokenAnd }
   or                         { \s -> TokenOr }
   $alpha [$alpha $digit \_ \']* { \s -> TokenVar s }
   $digit+                    { \s -> TokenInt (read s)}
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
      deriving Show

lanTokens = alexScanTokens

}
