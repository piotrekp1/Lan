module ParseDatatypes2 where
import Datatypes

data PBlock
    = PSkip
    | PExp0 PExp0
    | PDecl PDecl
    | PEBegin PExp0 PBlock
    | PDBegin PDecl PBlock
    deriving Show

data PExp0
    = PAsgn PMementry PExp0
    | PModAsgn PMementry String PExp0
    | PExp1 PExp1
    deriving Show

data PMementry
    = PVar Var
    | PArrEntry Var PArrIndexes
    deriving Show

data PArrIndexes
    = PSngInd PExp0
    | PMltInd PExp0 PArrIndexes
    deriving Show

data PExp1
    = PIf BExp0 BExp0 BExp0
    | PWhile BExp0 PExp1
    | BExp0 BExp0
    deriving Show

data BExp0
    = POr BExp1 BExp0
    | BExp1 BExp1
    deriving Show

data BExp1
    = PAnd PCmp BExp1
    | PCmp PCmp
    deriving Show

data PCmp
    = PCmpEq PGrOrLess PCmp
    | PGrOrLess PGrOrLess
    deriving Show

data PGrOrLess
    = PCmpExp Op ArExp0 ArExp0
    | ArExp0 ArExp0
    deriving Show

data ArExp0
    = Ar0Op Op ArExp0 ArExp1
    | ArExp1 ArExp1
    deriving Show

data ArExp1
    = Ar1Op Op ArExp1 PFooCall
    | PFooCall PFooCall
    deriving Show

data PFooCall
    = PFooCallArg PFooCall Factor
    | PreDefFooCallArg String Factor
    | Factor Factor
    deriving Show

data Factor
    = BrackPExp0 PExp0
    | MementryVal PMementry
    | PModInPl PMementry String
    | PBlock PBlock
    | PArrCall Factor PExp0
    | Value Value
    deriving Show

data Value
    = IntP Int
    | BoolP Bool
    | CharP Char
    | StringP String
    | ArrayP ArrData
    | PLambda Var PFooType PExp0
    deriving Show

data ArrData
    = ArrNothing
    | ArrEl  PExp0
    | ArrEls PExp0 ArrData
    deriving Show

data PFooType
    = PMltType PType PFooType
    | PType PType
    deriving Show

data PType
    = PRawType String
    | PTypeBrack PFooType
    | PTypeArray PFooType
    deriving Show


-- --------- Decl


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

