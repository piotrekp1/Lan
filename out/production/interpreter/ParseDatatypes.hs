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

data PExp0
    = PSkip
    | PAsgn Var PExp0
    | PScln PExp0 PExp0
    | PIfStmt BExp1 PExp0 PExp0
    | PWhile BExp1 PExp0
    | PBegin PDecl PExp0
    | PExp PExp
    deriving Show

data PDecl
    = PDSkip
    | PDecl Var Datatype
    | PDScln PDecl PDecl
    deriving Show

data BExp1
    = Or BExp1 BExp1
    | BExp2 BExp2
    deriving Show

data BExp2
    = And BExp2 BExp2
    | BBrack BExp1
    | PCmp PCmp
    | BVal Bool
    deriving Show

data PCmp
    = PCmpExp Ordering PExp PExp
    deriving Show