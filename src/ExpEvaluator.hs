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


-- assumes that tp doesnt consist FooT
evalFun' :: Function -> [Type] -> [Mementry]  -> StoreWithEnv Mementry
-- entire specification
evalFun' (RawExp exp) tp args = do -- evalExp' exp bylo todo: zrobic rozroznienie pomiedzy bindem a callem
    true_foo <- evalExp' exp
    case true_foo of
        (tp, (Foo envfunction2)) -> evalEnvFun' envfunction2 tp args
        otherwise -> return true_foo
-- partial specification
evalFun' foo@(ArgFun var fooin) tp@(argtp:resttp) [] = do
    env <- lift ask
    return $ (footypeFromArray tp, Foo (env, foo))
-- too many arguments (should never be called because there is static control now)
evalFun' (ArgFun var foo)  [] (arg:rest) = err $ "Call with too many parameters, function: " ++ var
-- many arguments
evalFun' (ArgFun var foo) (argtp_shouldbe:resttp) ((argtp, arg):rest) = do
    env <- lift ask
    store <- get
    let declareRes = declareDecl (FooDcl var argtp) env store
    case declareRes of
        Left message -> err message
        Right (newEnv, newStore) -> do
            let loc = fromJust $ lookup var newEnv
            modify (overwriteMem loc (argtp, arg))
            local (const newEnv) (evalFun' foo resttp rest)


evalEnvFun' :: EnvFunction -> Type -> [Mementry] -> StoreWithEnv Mementry
evalEnvFun' (env, function) tp args = local (const env) (evalFun' function tps args)
    where tps = footypes tp

withDeclared :: Decl -> StoreWithEnv a -> StoreWithEnv a
withDeclared decl prog = do
    store <- get
    env <- lift ask
    let declareRes = declareDecl decl env store
    case declareRes of
        Left message -> err message
        Right (newEnv, newStore) -> do
            modify (const newStore)
            local (const newEnv) prog

-- assumes that there is a function in mementry (doesn't do type checking)
callFunction :: [Exp] -> Mementry -> StoreWithEnv Mementry
callFunction argexps (tp, Foo envfunction@(env, foo)) = do
    case foo of
        (RawExp rawexp) -> do
            true_foo <- evalExp' rawexp
            case true_foo of
                (tp, (Foo envfunction2)) -> do
                    args <- sequence (map evalExp' argexps)
                    evalEnvFun' envfunction2 tp args
                otherwise -> return true_foo
        otherwise -> do
            args <- sequence (map evalExp' argexps)
            evalEnvFun' envfunction tp args
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
evalExp' (EVar varName) = do
    loc <- getLoc varName
    res@(tp, dt) <- getNonEmptyValue loc
    case dt of
        Foo envfunction -> evalEnvFun' envfunction tp []  -- todo: obsługa niezaalokowanej pamięci
        otherwise -> return res
-- skip
evalExp' Skip = return $ (Ign, Undefined)
-- overwriting a variable
evalExp' (SAsgn varName exp) = do
    loc <- getLoc varName
    (tp, val) <- getValue loc
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
evalExp' (FooCall fooname argexps) = pure fooname >>= getLoc >>= getValue >>= callFunction argexps
-- Function bind
evalExp' (FooBind fooname fooargNames) = do
    loc <- getLoc fooname
    (tp, val) <- getValue loc
    let (Foo envfunction) = val
    args <- sequence (map evalExp' fooargNames)
    evalEnvFun' envfunction tp args
-- lambda
evalExp' lambda@(SLam (SLamCon var vartype fooexp)) = do
    env <- ask
    tp <- checkExp' lambda
    let funEnv = Foo (env, fooFromExpVars [var] fooexp)
    return (tp, funEnv)
-- lambda call
evalExp' (LamCall lam argexps) = pure lam >>= evalExp' . SLam >>= callFunction argexps



evalExpEither :: Exp -> Either Exception Mementry
evalExpEither exp = runReaderT (evalStateT (evalExp' exp) DMap.empty) []

execStmt :: Exp -> Either Exception (Mementry, Store)
execStmt stmt = runReaderT (runStateT (evalExp' stmt) DMap.empty) []

execStmtEnv :: Env -> Exp -> Either Exception (Mementry, Store)
execStmtEnv env stmt = runReaderT (runStateT (evalExp' stmt) DMap.empty) env

{- DECL -}
footypes :: Type -> [Type] -- array consisting of only basic types
footypes (IntT) = [IntT]
footypes (BoolT) = [BoolT]
footypes (FooBr tp) = [tp]
footypes (FooT type1 type2) = (footypes type1) ++ (footypes type2)

fooFromExpVars :: [Var] -> Exp -> Function
fooFromExpVars [] exp = RawExp exp
fooFromExpVars (x:xs) exp =  (ArgFun x) (fooFromExpVars xs exp)

declareDecl' :: Decl -> StateT (Env, Store) (Either Exception)  ()
-- semicolon in decl
declareDecl' (DScln decl1 decl2) = do
    declareDecl' decl1
    declareDecl' decl2
-- skip
declareDecl' (DSkip)  = return ()
-- fun declaration
declareDecl' (FooDcl varName tp) = do
    (env, store) <- get
    let newStore = DMap.insert (nextLoc store) (tp, Undefined) store
    let newEnv = declareEnvVar varName (nextLoc store) env
    modify (const (newEnv, newStore))
-- function definition
declareDecl' (FooDfn fooname vars expr) = do
    (tp, dt) <- getNonEmptyStateStoredVar fooname
    loc <- getStateStoredLoc fooname
    (env, store) <- get
    let funEnv = Foo (env, fooFromExpVars vars expr)
    TransSt.modify (\(env, store) -> (env, DMap.insert loc (tp, funEnv) store))

declareDecl :: Decl -> Env -> Store -> Either Exception (Env, Store)
declareDecl decl env store = execStateT (declareDecl' decl) (env, store)

declareVars' :: Type -> [Var] -> StateT (Env, Store) (Either Exception)  ()
declareVars' tp vars = do
    let dcls = map (\(var, varType) -> declareDecl' (FooDcl var varType)) (zip vars (footypes tp))
    sequence_ dcls

declareVars :: Type -> [Var] -> Env -> Store -> Either Exception (Env, Store)
declareVars tp vars env store = execStateT (declareVars' tp vars) (env, store)

declareVar :: Type -> Var -> Env -> Store -> Either Exception (Env, Store) -- todo: bez overheadu w postaci tablicy
declareVar tp var = declareVars tp [var]








