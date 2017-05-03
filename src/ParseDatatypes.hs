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

data PBlock
    = PBegin PDecl PSntnc
    | PDecl PDecl
    | PSntnc PSntnc
    deriving Show

data PSntnc
    = PSkip
    | PScln PSntnc PSntnc
    | PExpFoo PExpFoo
      deriving Show

data PExpFoo
    = PFooCall Var PFooArgs
    | PExp0 PExp0
     deriving Show

data PExp0
    = PAsgn Var PExpFoo
    | PIfStmt BExp1 PExpFoo PExpFoo
    | PWhile BExp1 PExpFoo
    | PExp PExp
    | SntBrack PSntnc
    | PFooBrack PExpFoo
    deriving Show

data PFooArgs
    = PSngArg PExp0
    | PMltArgs PExp0 PFooArgs
    deriving Show

data PDecl
    = PDSkip
    | PSingDecl Var PFooType -- datatype Name
    | PDScln PDecl PDecl
    deriving Show

data PFooType
    = PType String
    | PMltType String PFooType
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