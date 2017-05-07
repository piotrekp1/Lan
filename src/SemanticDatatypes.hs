module SemanticDatatypes where
import Datatypes
import qualified Data.Map as DMap
import Control.Monad.Reader
import Control.Monad.State

type Loc = Int

type Env = [(Var, Loc)]
type Store = DMap.Map Loc Mementry

type Exception = String

type StoreWithEnv = StateT Store (ReaderT Env (Either Exception))
type Mementry = (Type, Datatype)
data Function
    = RawExp Exp
    | ArgFun Var Function

instance Show Function where
  show (RawExp rawexp) = "rawexp: " ++ show rawexp
  show (ArgFun var foo) = show var ++ " -> " ++ show foo

type EnvFunction = (Env, Function)


data Type
    = IntT
    | BoolT
    | FooT Type Type
    | Array Type
    | Any
    | Ign -- type used to return from statements that shouldn't have ret value (e.g. print)

instance Eq Type where
     IntT == IntT = True
     BoolT == BoolT = True
     FooT f1_tp1 f1_tp2 == FooT f2_tp1 f2_tp2 = f1_tp1 == f2_tp1 && f1_tp2 == f2_tp2
     (Array tp1) == (Array tp2) = tp1 == tp2
     Any == tp = True
     tp == Any = True
     Ign == Ign = True
     tp1 == tp2 = False

optype :: Op -> Type
optype (OpAdd) = FooT IntT (FooT IntT IntT)
optype (OpMul) = FooT IntT (FooT IntT IntT)
optype (OpSub) = FooT IntT (FooT IntT IntT)
optype (OpDiv) = FooT IntT (FooT IntT IntT)
optype (OpOr) = FooT BoolT (FooT BoolT BoolT)
optype (OpAnd) = FooT BoolT (FooT BoolT BoolT)
optype (OpEQ) = FooT IntT (FooT IntT BoolT)
optype (OpLT) = FooT IntT (FooT IntT BoolT)
optype (OpGT) = FooT IntT (FooT IntT BoolT)

instance Show Type where
    show (IntT) = "Int"
    show (BoolT) = "Bool"
    show (FooT tp1 tp2) = "(" ++ show tp1 ++ " -> " ++ show tp2 ++ ")"
    show (Ign) = "Ign"
    show (Array tp) = "[" ++ show tp ++ "]"
data Datatype
    = Num Int
    | BoolD Bool
    | Foo EnvFunction
    | DataArray [Datatype]
    | Undefined
    deriving (Show)


data Exp
    = EVal (Type, Datatype)
    | EOp Op Exp Exp
    | EArrDef [Exp]
    | EVar Var
    | ELet Var Exp Exp
    | Skip
    | SAsgn Var Exp
    | SArrAsgn Var [Exp] Exp
    | SScln Exp Exp
    | SIfStmt Exp Exp Exp
    | SWhile Exp Exp
    | SBegin Decl Exp
    | FooCall Var [Exp]
    | FooBind Var [Exp]
    | SLam SLam
    | LamCall SLam [Exp]
    | EArrCall Exp Exp
    deriving (Show)

data SLam
    = SLamCon Var Type Exp deriving Show

data BExp =
     BEBool Bool
    | BEOp Op BExp BExp
    | BEVar Var
    | BCmp Ordering Exp Exp
    deriving (Show)

data Decl =
      DSkip
    | DDecl Var Datatype -- standardowo inicjalizowane na jakąś wartość
    | DScln Decl Decl
    | FooDcl Var Type
    | FooDfn Var [Var] Exp
    deriving (Show)