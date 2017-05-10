{
module Gramma  where
import Data.Char
import ParseDatatypes2
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
      char            { TokenChar $$ }
      string          { TokenString $$ }
      var             { TokenVar $$ }
      'modInPl'       { TokenModInPlace $$ }
      'mod='          { TokenMod $$ }
      'preDefFoo'     { TokenPreDefFoo $$ }
      '!='            { TokenNotEq }
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
      '\\'            { TokenBackslash }
      '_'             { TokenDeclSep }
      '[:'            { TokenArrDefOB }
      ':]'            { TokenArrDefCB }
      '['             { TokenArrayOB }
      ']'             { TokenArrayCB }

%%




PBlock :                               { PSkip }
      | PExp0                          { PExp0 $1 }
      | PDecl                          { PDecl $1 }
      | PExp0 separator PBlock         { PEBegin $1 $3 }
      | PDecl separator PBlock         { PDBegin $1 $3 }


PExp0 : PMementry '=' PExp0           { PAsgn $1 $3 }
      | PMementry 'mod=' PExp0        { PModAsgn $1 $2 $3 }
      | PExp1                         { PExp1 $1 }

PMementry : var                       { PVar $1 }
      | var PArrIndexes               { PArrEntry $1 $2}

PExp1 : if BExp0 then BExp0 else BExp0 { PIf $2 $4 $6 }
      | while BExp0 ':' PExp1         { PWhile $2 $4 }
      | BExp0                         { BExp0 $1 }

BExp0 : BExp1 or BExp0                { POr $1 $3 }
      | BExp1                         { BExp1 $1 }

BExp1 : PCmp and BExp1                { PAnd $1 $3 }
      | PCmp                          { PCmp $1 }

PCmp  : PGrOrLess '==' PCmp           { PCmpEq $1 $3 }
      | PGrOrLess                     { PGrOrLess $1 }

PGrOrLess :  ArExp0 '>' ArExp0        { PCmpExp OpGT $1 $3 }
      | ArExp0 '<' ArExp0             { PCmpExp OpLT $1 $3 }
      | ArExp0                        { ArExp0 $1 }

ArExp0 : ArExp0 '+' ArExp1            { Ar0Op OpAdd $1 $3 }
      | ArExp0 '-' ArExp1             { Ar0Op OpSub $1 $3 }
      | ArExp1                        { ArExp1 $1 }

ArExp1 : ArExp1 '*' PFooCall           { Ar1Op OpMul $1 $3 }
      | ArExp1 '/' PFooCall            { Ar1Op OpDiv $1 $3 }
      | PFooCall                      { PFooCall $1 }

PFooCall : PFooCall Factor            { PFooCallArg $1 $2 }
      | 'preDefFoo' Factor            { PreDefFooCallArg $1 $2 }
      | Factor                        { Factor $1 }

Factor : '(' PExp0 ')'                { BrackPExp0 $2 }
      | PMementry                     { MementryVal $1 }
      | PMementry 'modInPl'           { PModInPl $1 $2}
      | '{' PBlock '}'                { PBlock $2 }
      | Factor '[' PExp0 ']'          { PArrCall $1 $3 }
      | Value                         { Value $1 }

Value : int                           { IntP $1 }
      | bool                          { BoolP $1 }
      | char                          { CharP $1 }
      | string                        { StringP $1 }
      | '[:' ArrData ':]'             { ArrayP $2 }
      | '[:' ':]'                     { ArrayP ArrNothing }
      | '\\' var '::' PFooType ':' PExp0 { PLambda $2 $4 $6}

ArrData : PExp0 ',' ArrData             { ArrEls $1 $3 }
      | PExp0                          { ArrEl $1 }

PFooType : PType arrow PFooType    { PMltType $1 $3 }
      | PType                         { PType $1 }

PType :  type                         { PRawType  $1 }
      | '(' PFooType ')'              { PTypeBrack $2 }
      | '[' PFooType ']'              { PTypeArray $2 }

PArrIndexes : '[' PExp0 ']'           { PSngInd $2 }
      | '[' PExp0 ']' PArrIndexes     { PMltInd $2 $4 }

-- ---------------------------- Decl

PDecl :                               { PDSkip }
      | let var '::' PFooType         { PSingDecl $2 $4}
      | PDecl separator PDecl         { PDScln $1 $3 }
      | PFooArgNames ':=' PExp0       { PFooDef $1 $3 } -- todo: powoduje kolizję gramatyki

PFooArgNames : var                    { PVarName $1 }
      | var PFooArgNames              { PVarNames $1 $2}


{
parseError :: [Token] -> a
parseError list = error ("Parse error" ++ show list)


}