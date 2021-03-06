module ExpEvaluator where


import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
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
evalInfix (OpAdd) ((Array CharT), DataArray dt1) ((Array CharT), DataArray dt2) = return (Array CharT, DataArray (dt1 ++ dt2))
evalInfix (OpAdd) (arr@(Array CharT, DataArray dt1)) (tp, dt) = do
    showed_str <- showDatatype dt
    evalInfix OpAdd arr showed_str
evalInfix (OpAdd) (tp, dt) (arr@(Array CharT, DataArray dt1))  = do
    showed_str <- showDatatype dt
    evalInfix OpAdd showed_str arr
evalInfix (OpAdd) ((Array tp1), DataArray dt1) ((Array tp2), DataArray dt2) = return (Array tp1, DataArray (dt1 ++ dt2))
evalInfix (OpMul) ((Array tp), DataArray dt) ((IntT), Num n) = return (Array tp, DataArray (concat $ replicate n dt))
evalInfix op      ((IntT), (Num a)) ((IntT), (Num b)) = return (IntT, Num $ getIntOp op a b)
evalInfix op ((BoolT), (BoolD a)) ((BoolT), (BoolD b)) = return (BoolT, BoolD $ getBoolOp op a b)
evalInfix op arg1 arg2 = err $ "Internal error: didn't fit any expected pattern" ++ show op ++
    ", arg1: " ++ show arg1 ++
    ", arg2: " ++ show arg2 ++ "."



{- DECL -}
footypes :: Type -> [Type] -- array consisting of only basic types
footypes (FooT type1 type2) = (footypes type1) ++ (footypes type2)
footypes tp = [tp]

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


-- function always call as deep as they can.
-- so for example when function is a rawExp with a function inside
-- it will evaluate the rawExp and call the function inside.
--
-- assumes that tp doesnt consist FooT
evalFun' :: Function -> [Type] -> [Mementry]  -> StoreWithEnv Mementry
-- entire specification
evalFun' (RawExp exp) tp args = do
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

-- helper function used to handle many arguments
evalFooCallExps :: [Exp] -> Mementry -> StoreWithEnv Mementry
evalFooCallExps argexps fooentry@(tp, Foo envfunction) = mapM evalExp' argexps >>= evalEnvFun' fooentry
evalFooCallExps [] mementry = case mementry of
     (tp, Foo envfunction) -> evalEnvFun' mementry []
     otherwise -> return mementry
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
-- just a value
evalExp' (EVal b) = return b
-- infix operation
evalExp' (EOp op exp1 exp2) = do
    res1 <- evalExp' exp1
    res2 <- evalExp' exp2
    evalInfix op res1 res2
-- array inplace
evalExp' (EArrDef []) = return (Array AnyT, DataArray [] )
evalExp' (EArrDef (fst_exp:rest_exps)) = do
    (fst_tp, fst_el) <- evalExp' fst_exp
    (Array rest_tp, (DataArray rest_arr)) <- evalExp' $ EArrDef rest_exps
    return (Array fst_tp, DataArray (fst_el:rest_arr))
-- variable
evalExp' (EMementry (Variable varName)) = getNonEmptyValue varName >>= evalFooCallExps []
evalExp' (EMementry (ArrayEl varName [])) = evalExp' (EMementry (Variable varName))
evalExp' (EMementry (ArrayEl varName ind_exps)) = do
    let call_args = init ind_exps
    let last_arg = last ind_exps
    evalExp' (EArrCall (EMementry (ArrayEl varName call_args)) last_arg)
-- skip
evalExp' Skip = return $ (Ign, Undefined)
-- overwriting a variable
evalExp' (SAsgn (Variable varName) exp) = do
    loc <- getLoc varName
    tp <- getType varName
    res@(res_tp, res_val) <- evalExp' exp
    modify (DMap.insert loc (tp, res_val)) -- tp, not res_tp because of assigning empty array
    return (tp, res_val)
-- assign to an array
evalExp' (SAsgn mem@(ArrayEl varName ind_exps) exp) = do
    loc <- getLoc varName
    (tp, arr_val) <- getValue varName
    res@(res_tp, res_val) <- evalExp' exp
    inds <- expsToInts ind_exps
    new_val <- replaceNestedEl arr_val inds res_val
    modify (DMap.insert loc (tp, new_val))
    evalExp' (EMementry mem) -- necessary to return value that was assigned with correct type (defends against "AnyT" type)
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
evalExp' (SEBegin exp1 exp2) = do
    res1 <- evalExp' exp1
    res2@(tp2, val2) <- evalExp' exp2
    case tp2 of
        Ign -> return res1
        otherwise -> return res2
evalExp' (SDBegin decl stmt) = withDeclared decl (evalExp' stmt)
-- Function call
evalExp' (FooCall fooexp argexp) = evalExp' fooexp >>=  callFunction [argexp]
-- lambda
evalExp' lambda@(SLam var vartype fooexp) = do
    env <- lift ask
    tp <- checkExp' lambda
    let funEnv = Foo (env, fooFromExpVars [var] fooexp)
    return (tp, funEnv)
-- empty lambda
evalExp' (SEmptyLam fooexp) = do
    env <- lift ask
    tp <- checkExp' fooexp
    let funEnv = Foo (env, RawExp fooexp)
    return (tp, funEnv)
-- array call
evalExp' (EArrCall arrexp argexp) = do
    arr <- evalExp' arrexp
    (IntT, Num ind) <- evalExp' argexp
    getArrayEl arr ind
-- in place modulator ( ++, --)
evalExp' (OpMod eMemEntry op) = do
    retval <- evalExp' (EMementry eMemEntry)
    evalExp' (SAsgn eMemEntry (EOp op (EMementry eMemEntry) (EVal (IntT, Num 1))))
    return retval
-- predef functions
evalExp' (EPreDefFoo (Length) arr_exp) = do
    (tp,DataArray arr) <- evalExp' arr_exp
    return (IntT, Num $ length arr)
evalExp' (EPreDefFoo (Print) str_exp) = do
    (tp, DataArray dt_str) <- evalExp' str_exp
    tell $ [dataArrayToStr dt_str]
    return (Ign, Undefined)
evalExp' (EPreDefFoo (ShowFoo) exp) = do
    res@(res_tp, res_val) <- evalExp' exp
    case res_tp of
        (Array CharT) -> return res
        otherwise -> showDatatype res_val

-- assumes that the argument is SBegin
runExp' :: Exp -> StoreWithEnv Env
runExp' (SDBegin decl stmt) = do
    newEnv <- declareDecl' decl
    local (const newEnv) (evalExp' stmt)
    return newEnv
runExp' exp = do
    env <- ask
    evalExp' exp
    return env

runBlock :: Env -> Store -> Exp -> Either Exception ((Env, Store), [String])
runBlock env store exp = runWriterT $ runReaderT (runStateT (runExp' exp) store) env

checkBlock :: Env -> Store -> Exp -> Either Exception ((Type,Store), [String])
checkBlock env store exp = runWriterT $ runReaderT (runStateT (checkExp' exp) store) env

execStmt :: Exp -> Either Exception ((Mementry, Store), [String])
execStmt stmt = execStoreWithEnv $ evalExp' stmt

-- assumes that exps returns an int
expsToInts :: [Exp] -> StoreWithEnv [Int]
expsToInts [] = return []
expsToInts (exp:restexp) = do
    results <- expsToInts restexp
    (IntT, Num val) <- evalExp' exp
    return $ val:results
