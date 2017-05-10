module SemanticDatatypes where
import Datatypes
import qualified Data.Map as DMap
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

type Loc = Int

type Env = [(Var, Loc)]
type Store = DMap.Map Loc Mementry

type Exception = String

type StoreWithEnv = StateT Store (ReaderT Env (WriterT [String] (Either Exception)))
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
    | CharT
    | FooT Type Type
    | Array Type
    | AnyT
    | Ign -- type used to return from statements that shouldn't have ret value (e.g. print)

instance Eq Type where
     IntT == IntT = True
     BoolT == BoolT = True
     CharT == CharT = True
     FooT f1_tp1 f1_tp2 == FooT f2_tp1 f2_tp2 = f1_tp1 == f2_tp1 && f1_tp2 == f2_tp2
     (Array tp1) == (Array tp2) = tp1 == tp2
     AnyT == tp = True
     tp == AnyT = True
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
    show (CharT) = "Char"
    show (FooT tp1 tp2) = "(" ++ show tp1 ++ " -> " ++ show tp2 ++ ")"
    show (Ign) = "Ign"
    show (Array tp) = "[" ++ show tp ++ "]"
data Datatype
    = Num Int
    | BoolD Bool
    | CharD Char
    | Foo EnvFunction
    | DataArray [Datatype]
    | Undefined
    deriving (Show)


data Exp
    = EVal (Type, Datatype)
    | EOp Op Exp Exp
    | EArrDef [Exp]
    | EMementry EMementry
    | ELet Var Exp Exp
    | Skip
    | SAsgn EMementry Exp
    | SScln Exp Exp
    | SIfStmt Exp Exp Exp
    | SWhile Exp Exp
    | SEBegin Exp Exp
    | SDBegin Decl Exp
    | FooCall Exp Exp
 -- | FooBind Var [Exp]
    | SLam Var Type Exp
    | OpMod EMementry Op
    | EArrCall Exp Exp
    | EPreDefFoo PreDefFoo Exp
    deriving (Show)

data EMementry
    = Variable Var
    | ArrayEl Var [Exp]
    deriving Show

data PreDefFoo -- todo: dodac semantykę
    = Print
    | Length
    | ShowFoo
    deriving Show

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