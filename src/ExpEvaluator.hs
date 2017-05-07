module ExpEvaluator where


import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Trans.State as TransSt
import Data.Maybe
import Datatypes
import qualified Data.Map as DMap
import StaticChecker
import SemanticDatatypes
import Data.Ord
import Memory
import Utils


getIntOp :: Op -> Int -> Int -> Int
getIntOp OpAdd = (+)
getIntOp OpMul = (*)
getIntOp OpSub = (-)
getIntOp OpDiv = div

getBoolOp :: Op -> Bool -> Bool -> Bool
getBoolOp OpOr = (||)
getBoolOp OpAnd = (&&)


-- doesn't do type checking
evalInfix :: Op -> Mementry -> Mementry -> StoreWithEnv Mementry
evalInfix (OpEQ)  ((IntT), (Num a)) ((IntT), (Num b)) = return (BoolT, BoolD $ a == b)
evalInfix (OpEQ)  ((BoolT), (BoolD a)) ((BoolT), (BoolD b)) = return (BoolT, BoolD $ a == b)
evalInfix (OpLT)  ((IntT), (Num a)) ((IntT), (Num b)) = return (BoolT, BoolD $ a < b)
evalInfix (OpGT)  ((IntT), (Num a)) ((IntT), (Num b)) = return (BoolT, BoolD $ a > b)
evalInfix (OpDiv) ((IntT), (Num a)) ((IntT), (Num 0)) = err "Tried to divide by 0"
evalInfix op      ((IntT), (Num a)) ((IntT), (Num b)) = return (IntT, Num $ getIntOp op a b)
evalInfix op ((BoolT), (BoolD a)) ((BoolT), (BoolD b)) = return (BoolT, BoolD $ getBoolOp op a b)
evalInfix op arg1 arg2 = err $ "Internal error: didn't fit any expected pattern" ++ show op ++
    ", arg1: " ++ show arg1 ++
    ", arg2: " ++ show arg2 ++ "."



{- DECL -}
footypes :: Type -> [Type] -- array consisting of only basic types
footypes (IntT) = [IntT]
footypes (BoolT) = [BoolT]
footypes (FooBr tp) = [tp]
footypes (FooT type1 type2) = (footypes type1) ++ (footypes type2)

fooFromExpVars :: [Var] -> Exp -> Function
fooFromExpVars [] exp = RawExp exp
fooFromExpVars (x:xs) exp =  (ArgFun x) (fooFromExpVars xs exp)


declareDecl' :: Decl -> StoreWithEnv Env
-- semicolon in decl
declareDecl' (DScln decl1 decl2) = do
    newEnv <- declareDecl' decl1
    local (const newEnv) $ declareDecl' decl2
-- skip
declareDecl' (DSkip) = lift ask
-- fun declaration
declareDecl' foodcl@(FooDcl varName tp) = declareVar varName tp
-- function definition
declareDecl' (FooDfn fooname vars expr) = do
    loc <- getLoc fooname
    (tp, dt) <- getValue fooname
    env <- lift ask
    let funEnv = Foo (env, fooFromExpVars vars expr)
    modify (DMap.insert loc (tp, funEnv))
    lift ask


{- EXPS -}
withDeclared :: Decl -> StoreWithEnv a -> StoreWithEnv a
withDeclared decl prog = do
    newEnv <- declareDecl' decl
    local (const newEnv) prog


-- assumes that tp doesnt consist FooT
evalFun' :: Function -> [Type] -> [Mementry]  -> StoreWithEnv Mementry
-- entire specification
evalFun' (RawExp exp) tp args = do -- evalExp' exp bylo todo: zrobic rozroznienie pomiedzy bindem a callem
    true_foo <- evalExp' exp
    case true_foo of
        fooentry@(tp, (Foo envfunction2)) -> evalEnvFun' fooentry args
        otherwise -> return true_foo
-- partial specification
evalFun' foo@(ArgFun var fooin) tp@(argtp:resttp) [] = do
    env <- lift ask
    return $ (footypeFromArray tp, Foo (env, foo))
-- too many arguments (should never be called because there is static control now)
evalFun' (ArgFun var foo)  [] (arg:rest) = err $ "Call with too many parameters, function: " ++ var
-- many arguments
evalFun' (ArgFun var foo) (argtp_shouldbe:resttp) ((argtp, arg):rest) = withDeclared (FooDcl var argtp) (do
    loc <- getLoc var
    modify (overwriteMem loc (argtp, arg))
    evalFun' foo resttp rest
    )

evalEnvFun' :: Mementry -> [Mementry] -> StoreWithEnv Mementry
evalEnvFun' (tp, Foo (env, function)) args = local (const env) (evalFun' function tps args)
    where tps = footypes tp


evalFooCallExps :: [Exp] -> Mementry -> StoreWithEnv Mementry
evalFooCallExps argexps fooentry@(tp, Foo envfunction) = mapM evalExp' argexps >>= evalEnvFun' fooentry
evalFooCallExps [] fooentry@(tp, Foo envfunction) = evalEnvFun' fooentry []
evalFooCallExps [] mementry = return mementry
evalFooCallExps argexps mementry = err "too many parameters in a call"


-- assumes that there is a function in mementry (doesn't do type checking)
callFunction :: [Exp] -> Mementry -> StoreWithEnv Mementry
callFunction argexps fooentry@(tp, Foo envfunction@(env, foo)) = do
    case foo of
        (RawExp rawexp) -> evalExp' rawexp >>= evalFooCallExps argexps
        otherwise -> evalFooCallExps argexps fooentry
callFunction _ (tp, (Undefined)) = err "call to a function that was not defined"
callFunction argexps mementry = err $ show mementry ++ "\n ---- \n" ++ show argexps

evalExp' :: Exp -> StoreWithEnv Mementry
-- sama wartosc
evalExp' (EVal b) = return b
-- zlozenie wyrazen
evalExp' (EOp op exp1 exp2) = do
    res1 <- evalExp' exp1
    res2 <- evalExp' exp2
    evalInfix op res1 res2
-- ewaluacja zmiennej
evalExp' (EVar varName) = getNonEmptyValue varName >>= evalFooCallExps [] -- todo move everything to a foo call?
-- skip
evalExp' Skip = return $ (Ign, Undefined)
-- overwriting a variable
evalExp' (SAsgn varName exp) = do
    loc <- getLoc varName
    (tp, val) <- getValue varName
    res@(res_tp, res_val) <- evalExp' exp
    modify (DMap.insert loc (tp, res_val))
    return res
-- if
evalExp' (SIfStmt bexp stmt1 stmt2) = do
    env <- lift ask
    (tp, BoolD res) <- evalExp' bexp
    evalExp' $ if res then stmt1 else stmt2
-- while loop
evalExp' loop@(SWhile bexp stmt) = evalExp' (SScln stmt (SIfStmt bexp loop Skip))
-- Semicolon
evalExp' (SScln stmt1 stmt2) = do
    res1 <- evalExp' stmt1
    res2@(tp2, val2) <- evalExp' stmt2
    case tp2 of
        Ign -> return res1
        otherwise -> return res2
-- Begin block
evalExp' (SBegin decl stmt) = withDeclared decl (evalExp' stmt)
-- Function call
evalExp' (FooCall fooname argexps) = getValue fooname >>= callFunction argexps
-- Function bind
evalExp' (FooBind fooname argexps) = getValue fooname >>= evalFooCallExps argexps
-- lambda
evalExp' lambda@(SLam (SLamCon var vartype fooexp)) = do
    env <- lift ask
    tp <- checkExp' lambda
    let funEnv = Foo (env, fooFromExpVars [var] fooexp)
    return (tp, funEnv)
-- lambda call
evalExp' (LamCall lam argexps) = evalExp' (SLam lam) >>= callFunction argexps

execStmt :: Exp -> Either Exception (Mementry, Store)
execStmt stmt = execStoreWithEnv $ evalExp' stmt

evalExpEither :: Exp -> Either Exception Mementry
evalExpEither exp = runReaderT (evalStateT (evalExp' exp) DMap.empty) []


