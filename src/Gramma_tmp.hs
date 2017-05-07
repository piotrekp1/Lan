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
      ','             { TokenComma }
      and             { TokenAnd }
      or              { TokenOr }
      '=='            { TokenCmp }
      '>'             { TokenGT }
      '<'             { TokenLT }
      separator       { TokenSep }
      type            { TokenType $$ }
      ':'             { TokenTwoDots}
      '::'            { TokenDecl }
      arrow           { TokenArrow }
      ':='            { TokenDfn }
      bind            { TokenBind }
      '\\'             { TokenBackslash }
      '_'              { TokenDeclSep }
      '[|'              { TokenArrDefOB }
      '|]'              { TokenArrDefCB }
      '[:'              { TokenArrAsgnOB }
      ':]'              { TokenArrAsgnCB }
      '['               { TokenArrayOB }
      ']'               { TokenArrayCB }
%%



PBlock : PDecl '_' PSntnc             { PBegin $1 $3 }
      | PDecl                      { PDecl $1 }
      | PSntnc                     { PSntnc $1 }

PSntnc :                           { PSkip }
      | PSntnc separator PSntnc    { PScln $1 $3 }
      | PExp0                      { PExp0 $1 }

PExp0 : var '=' PExp0             { PAsgn $1 $3 }
      | var PAsgnIndexes '=' PExp0 { PArrAsgn $1 $2 $4}
      | if PExp0 then PExp0 else PExp0 { PIfStmt $2 $4 $6 }
      | while PExp0 ':' PExp0     { PWhile $2 $4 }
      | '{' PBlock '}'            { BlockBrack $2 }
      | Exp1                      { Exp1 $1 }
      | BExp1                     { BExp1 $1 }


Exp1  : Exp1 '+' Exp1           { E1Op OpAdd $1 $3 }
      | Exp1 '-' Exp1           { E1Op OpSub $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Term            { TOp OpMul $1 $3 }
      | Term '/' Term            { TOp OpDiv $1 $3 }
      | PExpFoo                  { PExpFoo $1 }


PExpFoo : var PFooArgs             { PFooCall $1 $2 }
      | bind var PFooArgs          { PFooBind $2 $3 }
      | bind var                   { PFooBind $2 PEmptArgs }
      | Factor                     { Factor $1 }
      | '(' Lambda ')' PFooArgs    { PLamCall $2 $4 }

Factor : '(' PExp0 ')'              { Brack $2 }
      | Value                       { Value $1 }
      | var                        { Var $1 }
      | PExp0 '[' PExp0 ']'         { ArrElCall $1 $3 }
      | Lambda                      { Lambda $1 }

Lambda : '\\' var '::' PFooType arrow PExp0 { PLam $2 $4 $6}

Value : int                         { IntP $1 }
      | bool                        { BoolP $1 }
      | '[|' ArrData '|]'             { ArrayP $2 }
      | '[|' '|]'                     { ArrayP ArrNothing }

ArrData : PExp0                     { ArrEl $1 }
      | PExp0 ',' ArrData          { ArrEls $1 $3 }

PAsgnIndexes : '[:' PExp0 ':]'          { PSngInd $2 }
      | '[:' PExp0 ':]' PAsgnIndexes   { PMltInd $2 $4 }

PFooArgs : Factor                  { PSngArg $1 }
      | Factor PFooArgs            { PMltArgs $1 $2 }

PDecl : let var '::' PFooType   { PSingDecl $2 $4}
      | PDecl separator PDecl    { PDScln $1 $3 }
      | PDecl separator          { PDScln $1 PDSkip }
      | PFooArgNames ':=' PExp0 { PFooDef $1 $3 } -- todo: powoduje kolizję gramatyki

PFooArgNames : var                 { PVarName $1 }
      | var PFooArgNames           { PVarNames $1 $2}

PFooType : PFooType arrow PFooType { PMltType $1 $3 }
      | type                     { PType  $1 }
      | '(' PFooType ')'         { PTypeBrack $2 }
      | '[' PFooType ']'         { PTypeArray $2 }

BExp1 : BExp1 or BExp1           { Or $1 $3 }
      | BExp2                    { BExp2 $1 }

BExp2 : BExp2 and BExp2          { And $1 $3 }
      | PCmp                     { PCmp $1 }
      | '(' PExp0 ')'            { BPExp0 $2 }

PCmp  : PExp0 '==' PExp0           { PCmpExp OpEQ $1 $3 }
      | PExp0 '>' PExp0            { PCmpExp OpGT $1 $3 }
      | PExp0 '<' PExp0            { PCmpExp OpLT $1 $3 }




{

parseError :: [Token] -> a
parseError list = error ("Parse error" ++ show list)


}