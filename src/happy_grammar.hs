{
module Parser (getTree) where
import Data.Char
import ParseDatatypes
import Datatypes
}

%name lang
%tokentype { Token }
%error { parseError }


%token
      while           { TokenWhile }
      '{'             { TokenLBracket }
      '}'             { TokenRBracket }
      if              { TokenIf }
      then            { TokenThen }
      else            { TokenElse }
      let             { TokenLet }
      in              { TokenIn }
      int             { TokenInt $$ }
      bool            { TokenBool $$ }
      var             { TokenVar $$ }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }
      and             { TokenAnd }
      or              { TokenOr }
      '=='            { TokenCmp }
      '>'             { TokenGT }
      '<'             { TokenLT }
      separator       { TokenSep }
      intType         { TokenIntType }
      ':'             { TokenTwoDots}
      '::'            { TokenDecl }
      true            { TokenTrue }
      false           { TokenFalse }
%%

PBlock : PDecl PSntnc              { PBegin $1 $2 }
      | PDecl                      { PDecl $1 }
      | PSntnc                     { PSntnc $1 }

PSntnc :                           { PSkip }
      | PSntnc separator PSntnc    { PScln $1 $3 }
      | PExp0                      { PExp0 $1 }

PExp0 : var '=' PExp0             { PAsgn $1 $3 }
      | if BExp1 then PExp0 else PExp0 { PIfStmt $2 $4 $6 }
      | while BExp1 ':' PExp0 { PWhile $2 $4 }
      | PExp                      { PExp $1 }
      | '{' PSntnc '}'            { SntBrack $2 }

PDecl : let var '::' intType     { PSingDecl $2 (Num 0)}
      | PDecl separator PDecl    { PDScln $1 $3 }
      | PDecl separator          { PDScln $1 PDSkip }

BExp1 : BExp1 or BExp1           { Or $1 $3 }
      | BExp2                    { BExp2 $1 }

BExp2 : BExp2 and BExp2          { And $1 $3 }
      | true                     { BVal True }
      | false                    { BVal False }
      | '(' BExp1 ')'            { BBrack $2 }
      | PCmp                     { PCmp $1 }

PCmp  : PExp '==' PExp           { PCmpExp EQ $1 $3 }
      | PExp '>' PExp            { PCmpExp GT $1 $3 }
      | PExp '<' PExp            { PCmpExp LT $1 $3 }

PExp  : let var '=' PExp in PExp { Let $2 $4 $6 }
      | Exp1                    { Exp1 $1 }

Exp1  : Exp1 '+' Exp1           { E1Op OpAdd $1 $3 }
      | Exp1 '-' Exp1           { E1Op OpSub $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { TOp OpMul $1 $3 }
      | Term '/' Factor         { TOp OpDiv $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' PExp ')'             { Brack $2 }


{-

BExp  : bool                      { BEBool $1 }
      | BExp Op BExp              { BEOp $2 $1 $3 }
      | var                       { BEVar $1 }
      | let var '=' BExp in BExp  { BELet $2 $4 $6 }

Op    : '&'                      { OpAnd }
      | '|'                      { OpOr }
-}

{

parseError :: [Token] -> a
parseError list = error ("Parse error" ++ show list)

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
      | TokenIntType
      | TokenDecl
      | TokenAnd
      | TokenOr
      | TokenCmp
      | TokenGT
      | TokenLT
      | TokenTrue
      | TokenFalse
      | TokenTwoDots
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('=':'=':cs) = TokenCmp : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (';':cs) = TokenSep : lexer cs
lexer ('\n':cs) = TokenSep : lexer cs
lexer ('{':cs) = TokenLBracket : lexer cs
lexer ('}':cs) = TokenRBracket : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer (':':':':cs) = TokenDecl : lexer cs
lexer (':':cs) = TokenTwoDots : lexer cs

numBool :: Int -> Bool
numBool 1 = True
numBool 0 = False

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs
{-


lexBool cs = TokenBool (numBool $ read num) : lexer rest
      where (num,rest) = span isDigit cs
-}

lexVar cs =
   case span isAlpha cs of
      ("while", rest) -> TokenWhile : lexer rest
      ("if", rest) -> TokenIf : lexer rest
      ("then", rest) -> TokenThen : lexer rest
      ("else", rest) -> TokenElse : lexer rest
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      ("Int",rest)  -> TokenIntType : lexer rest
      ("True", rest) -> TokenTrue : lexer rest
      ("False", rest) -> TokenFalse : lexer rest
      ("and", rest) -> TokenAnd : lexer rest
      ("or", rest) -> TokenOr : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

--main = getContents >>= print . calc . lexer
getTree = lang . lexer
}
