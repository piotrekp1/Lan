module ParseDatatypes where
import Datatypes

data PExp
      = Let String PExp PExp
      | Exp1 Exp1
      deriving Show

data Exp1
      = E1Op Op Exp1 Exp1
      | Term Term
      deriving Show

data Term
      = Factor Factor
      | TOp Op Term Factor
      deriving Show

data Factor
      = Int Int
      | Var String
      | Brack PExp
      deriving Show

data PStmt
    = PSkip
    | PAsgn Var PExp
    | PScln PStmt PStmt
    | PIfStmt PExp PStmt PStmt
    | PWhile PExp PStmt
    | PBegin PDecl PStmt
    deriving Show

data PDecl
    = PDSkip
    | PDecl Var Datatype
    | PDScln PDecl PDecl
    deriving Show