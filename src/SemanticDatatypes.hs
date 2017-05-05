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
    | FooBr Type
    | FooT Type Type
    | Ign -- type used to return from statements that shouldn't have ret value (e.g. print)
    deriving Eq

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
    show (FooBr tp) = "(" ++ show tp ++ ")"
    show (FooT tp1 tp2) = show tp1 ++ " -> " ++ show tp2
    show (Ign) = "Ign"
data Datatype
    = Num Int
    | BoolD Bool
    | Foo EnvFunction
    | Undefined
    deriving (Show)


data Exp
    = EVal (Type, Datatype)
    | EOp Op Exp Exp
    | EVar Var
    | ELet Var Exp Exp
    | Skip
    | SAsgn Var Exp
    | SScln Exp Exp
    | SIfStmt Exp Exp Exp
    | SWhile Exp Exp
    | SBegin Decl Exp
    | FooCall Var [Exp]
    | FooBind Var [Exp]
    deriving (Show)

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