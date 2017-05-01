module Datatypes where

data Op = OpAdd | OpMul | OpSub | OpDiv deriving (Show)
data Datatype = Num Int deriving (Show)

type Var = String

data Exp =
      EInt Int
    | EOp Op Exp Exp
    | EVar Var
    | ELet Var Exp Exp
    | Exp1 Exp
    | Term Exp
    deriving (Show)

data Stmt =
      Skip
    | SAsgn Var Exp
    | SScln Stmt Stmt
    | SIfStmt Exp Stmt Stmt
    | SWhile Exp Stmt
    | SBegin Decl Stmt
    deriving (Show)

data Decl =
      DDecl Var Datatype -- standardowo inicjalizowane na jakąś wartość
    | DScln Decl Decl
    deriving (Show)
