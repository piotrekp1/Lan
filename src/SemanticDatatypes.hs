module SemanticDatatypes where
import Datatypes

data Exp =
      EInt Int
    | EOp Op Exp Exp
    | EVar Var
    | ELet Var Exp Exp
    deriving (Show)

data BExp =
     BEBool Bool
    | BEOp Op BExp BExp
    | BEVar Var
    | BCmp Ordering Exp Exp
    deriving (Show)

data Stmt =
      Skip
    | SAsgn Var Exp
    | SScln Stmt Stmt
    | SIfStmt BExp Stmt Stmt
    | SWhile BExp Stmt
    | SBegin Decl Stmt
    deriving (Show)

data Decl =
      DSkip
    | DDecl Var Datatype -- standardowo inicjalizowane na jakąś wartość
    | DScln Decl Decl
    deriving (Show)