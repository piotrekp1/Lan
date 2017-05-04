{
module Gramma  where
import Data.Char
import ParseDatatypes
import Datatypes
import Tokens
}

%name lanParse
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
      type            { TokenType $$ }
      ':'             { TokenTwoDots}
      '::'            { TokenDecl }
      true            { TokenTrue }
      false           { TokenFalse }
      arrow           { TokenArrow }
      ':='            { TokenDfn }
      bind            { TokenBind }
%%



PBlock : PDecl  PSntnc             { PBegin $1 $2 }
      | PDecl                      { PDecl $1 }
      | PSntnc                     { PSntnc $1 }

PSntnc :                           { PSkip }
      | PSntnc separator PSntnc    { PScln $1 $3 }
      | PExpFoo                    { PExpFoo $1 }

PExpFoo : var PFooArgs             { PFooCall $1 $2 }
      | PExp0                      { PExp0 $1 }
      | bind var PFooArgs          { PFooBind $2 $3 }
      | bind var                   { PFooBind $2 PEmptArgs }

PExp0 : var '=' PExpFoo             { PAsgn $1 $3 }
      | if BExp1 then PExpFoo else PExpFoo { PIfStmt $2 $4 $6 }
      | while BExp1 ':' PExpFoo { PWhile $2 $4 }
      | PExp                      { PExp $1 }
      | '{' PSntnc '}'            { SntBrack $2 }
      | '(' PExpFoo ')'           { PFooBrack $2 }

PFooArgs : PExp0                  { PSngArg $1 }
      | PExp0 PFooArgs            { PMltArgs $1 $2 }

PDecl : let var '::' PFooType   { PSingDecl $2 $4}
      | PDecl separator PDecl    { PDScln $1 $3 }
      | PDecl separator          { PDScln $1 PDSkip }
      | PFooArgNames ':=' PExpFoo { PFooDef $1 $3 } -- todo: powoduje kolizję gramatyki


PFooArgNames : var                 { PVarName $1 }
      | var PFooArgNames           { PVarNames $1 $2}


PFooType : PFooType arrow PFooType { PMltType $1 $3 }
      | type                     { PType  $1 }
      | '(' PFooType ')'         { PTypeBrack $2 }

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
      | '(' PExpFoo ')'         { Brack $2 }

{
{-

showTokenType :: Token -> String
showTokenType (TokenType str) = map (filter (/='"')) str
-}

parseError :: [Token] -> a
parseError list = error ("Parse error" ++ show list)


}