module SemanticDatatypes where
import Datatypes
import qualified Data.Map as DMap
import Control.Monad.Reader
import Control.Monad.State

type Loc = Int

type Env = [(Var, Loc)]
type Store = DMap.Map Loc (Type, Maybe Datatype)

type StoreWithEnv = StateT Store (Reader Env)

data Function
    = RawExp Exp
    | ArgFun (Datatype -> Function)

instance Show Function where
  show (RawExp rawexp) = show rawexp
  show (ArgFun argfun) = "argfun"

type EnvFunction = ((Env, [Var]), Function)


data Type
    = IntT
    | BoolT
    | FooT Type Type
    deriving Show

data Datatype
    = Num Int
    | BoolD Bool
    | Foo EnvFunction
    deriving (Show)


data Exp =
      EInt Int
    | EOp Op Exp Exp
    | EVar Var
    | ELet Var Exp Exp
    | Skip
    | SAsgn Var Exp
    | SScln Exp Exp
    | SIfStmt BExp Exp Exp
    | SWhile BExp Exp
    | SBegin Decl Exp
    | FooCall Var [Exp]
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