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
      = PExpFoo PExpFoo
      | TOp Op Term PExpFoo
      deriving Show

data Factor
      = Value Value
      | Var String
      | Brack PExp0
      | FFooCall PExpFoo
      | Lambda Lambda
      deriving Show

data Lambda
     = PLam Var PFooType PExp0
     deriving Show

data Value
     = IntP Int
     | BoolP Bool
     deriving Show

data PBlock
    = PBegin PDecl PSntnc
    | PDecl PDecl
    | PSntnc PSntnc
    deriving Show

data PSntnc
    = PSkip
    | PScln PSntnc PSntnc
    | PExp0 PExp0
      deriving Show

data PExpFoo
    = PFooCall Var PFooArgs
    | PFooBind Var PFooArgs
    | PLamCall Lambda PFooArgs
    | Factor Factor
     deriving Show

data PExp0
    = PAsgn Var PExp0
    | PIfStmt PExp0 PExp0 PExp0
    | PWhile PExp0 PExp0
    | PExp PExp
    | SntBrack PSntnc
    | BExp1 BExp1
    deriving Show

data PFooArgs
    = PSngArg Factor
    | PMltArgs Factor PFooArgs
    | PEmptArgs
    deriving Show

data PDecl
    = PDSkip
    | PSingDecl Var PFooType -- datatype Name
    | PDScln PDecl PDecl
    | PFooDef PFooArgNames PExp0
    deriving Show

data PFooArgNames
    = PVarName Var
    | PVarNames Var PFooArgNames
    deriving Show

data PFooType
    = PType String
    | PMltType PFooType PFooType
    | PTypeBrack PFooType
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
    | BPExp0 PExp0
    deriving Show

data PCmp
    = PCmpExp Op PExp0 PExp0
    deriving Show