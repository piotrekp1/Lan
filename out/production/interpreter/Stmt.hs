module Stmt where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Datatypes
import qualified Data.Map as DMap
import SemanticDatatypes
import Gramma
import Tokens
import DTCleaner
import Data.Ord
import Memory


err :: String -> StoreWithEnv a
err = lift . lift . Left

typeMismatch :: Type -> Type -> String
typeMismatch expected got = "Expected: " ++ show expected ++ ", Got: " ++ show got ++ ". "

assertTrue :: Bool -> String -> StoreWithEnv Type
assertTrue predicate message = if not predicate then err message else return Ign -- todo upewnic się że to nie szkodzi



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

evalInfixType :: Op -> Type -> Type -> StoreWithEnv Type
evalInfixType (OpEQ) (IntT) (IntT) = return BoolT
evalInfixType (OpEQ) (BoolT) (BoolT) = return BoolT
evalInfixType (OpLT) (IntT) (IntT) = return BoolT
evalInfixType (OpGT) (IntT) (IntT) = return BoolT
evalInfixType (OpAdd) (IntT) (IntT) = return IntT
evalInfixType (OpSub) (IntT) (IntT) = return IntT
evalInfixType (OpMul) (IntT) (IntT) = return IntT
evalInfixType (OpDiv) (IntT) (IntT) = return IntT
evalInfixType (OpOr) (BoolT) (BoolT) = return BoolT
evalInfixType (OpAnd) (BoolT) (BoolT) = return BoolT
evalInfixType op tp1 tp2 = err $ ("Mismatched types in infix operation " ++ show op ++
     ", arg1 type: " ++ show tp1 ++
     ", arg2 type: " ++ show tp2 ++ ". ")

--assumes there is no FooT in array
footypeFromArray :: [Type] -> Type
footypeFromArray [t] = t
footypeFromArray (t:rest) = FooT t (footypeFromArray rest)

-- assumes that tp doesnt consist FooT
evalFun' :: Function -> [Type] -> [Mementry]  -> StoreWithEnv Mementry
-- entire specification
evalFun' (RawExp exp) tp args = evalExp' exp
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
    let (newEnv, newStore) = declareDecl (FooDcl var argtp) env store
    let loc = fromJust $ lookup var newEnv
    modify (overwriteMem loc (argtp, arg))
    local (const newEnv) (evalFun' foo resttp rest)


evalEnvFun' :: EnvFunction -> Type -> [Mementry] -> StoreWithEnv Mementry
evalEnvFun' (env, function) tp args = local (const env) (evalFun' function tps args)
    where tps = footypes tp

withVar' :: Var -> (Loc -> StoreWithEnv a) -> StoreWithEnv a
withVar' varName ifThereIs = do
    env <- lift ask
    let err_message = "Use of undeclared variable: " ++ varName ++ ". "
    case lookup varName env of
        Nothing -> err err_message
        Just loc -> ifThereIs loc

getValue :: Loc -> StoreWithEnv Mementry
getValue loc = do
    store <- get
    let err_message = "tried to get memory from not DECLARED variable but with set loc. "
    case DMap.lookup loc store of
         Nothing -> err err_message
         Just tp_dt -> return tp_dt

getTypeLoc :: Loc -> StoreWithEnv Type
getTypeLoc loc = do
    (tp, val) <- getValue loc
    return tp

getNonEmptyValue :: Loc -> StoreWithEnv Mementry
getNonEmptyValue loc = do
    res@(tp, dt) <- getValue loc
    let err_message = "tried to get memory from not DEFINED variable. "
    case dt of
        Undefined -> err err_message
        otherwise -> return res


checkExp' :: Exp -> StoreWithEnv Type
-- sama wartosc
checkExp' (EVal (tp, dt)) = return tp
-- zlozenie wyrazen
checkExp' (EOp op exp1 exp2) = do
    type1 <- checkExp' exp1
    type2 <- checkExp' exp2
    evalInfixType op type1 type2
-- ewaluacja zmiennej
checkExp' (EVar varName) = withVar' varName getTypeLoc
-- skip
checkExp' (Skip) = return Ign
-- overwriting a variable
checkExp' (SAsgn varName exp) = withVar' varName (\loc -> do
    res_tp <- checkExp' exp
    tp <- getTypeLoc loc -- checks if variable is allocated
    assertTrue (res_tp == tp) ("Different types in assignment in " ++ varName ++ ". " ++ (typeMismatch tp res_tp))
    return res_tp
    )
-- if
checkExp' (SIfStmt bexp stmt1 stmt2) = do
    env <- lift ask
    cond_tp <- checkExp' bexp -- todo: kontrola typu
    assertTrue (cond_tp == BoolT) ("Used non-boolean expression in if or while condition. ")
    tp1 <- checkExp' stmt1
    tp2 <- checkExp' stmt2
    assertTrue (tp1 == tp2) ("Expressions in if have different types. ")
    return tp1
-- while loop
checkExp' loop@(SWhile bexp stmt) = checkExp' stmt -- checkExp' (SScln stmt (SIfStmt bexp loop Skip)) -- do while syntax
-- Semicolon
checkExp' (SScln stmt1 stmt2) = do
    tp1 <- checkExp' stmt1
    tp2 <- checkExp' stmt2
    return $ if tp2 == Ign then tp1 else tp2
-- Begin block
checkExp' (SBegin decl stmt) = do
    store <- get
    env <- lift ask
    let (newEnv, newStore) = declareDecl decl env store
    modify (const newStore)
    local (const newEnv) $ checkExp' stmt
-- Function call
checkExp' (FooCall fooname args) = withVar' fooname (\l -> getTypeLoc l >>= checkFooCallType' fooname args)
-- Function bind
checkExp' (FooBind fooname args) = checkExp' (FooCall fooname args)
-- Lambda
checkExp' (SLam var vartype fooexp) = do
    store <- get
    env <- lift ask
    let (newEnv, newStore) = declareDecl (FooDcl var vartype) env store


unbrackFoo :: Type -> Type
unbrackFoo (FooBr tp) = tp
unbrackFoo a = a

checkFooCallType' :: String -> [Exp] -> Type -> StoreWithEnv Type
checkFooCallType' fooname [] footype = return footype
checkFooCallType' fooname (firstarg:rest) footype = do
    case footype of
        FooT from_tp to_tp -> do
            firstarg_tp <- checkExp' firstarg
            assertTrue (unbrackFoo from_tp == unbrackFoo firstarg_tp) $ "Wrong argument type in call of a function named: " ++ fooname ++ " - " ++ (typeMismatch from_tp firstarg_tp)
            checkFooCallType' fooname rest to_tp
        otherwise -> err $ "Too many parameters in a call, variable: " ++ fooname ++ " - but with parameters given. "


evalExp' :: Exp -> StoreWithEnv Mementry
-- sama wartosc
evalExp' (EVal b) = return b
-- zlozenie wyrazen
evalExp' (EOp op exp1 exp2) = do
    res1 <- evalExp' exp1
    res2 <- evalExp' exp2
    evalInfix op res1 res2
-- ewaluacja zmiennej
evalExp' (EVar varName) = withVar' varName (\loc -> do
    res@(tp, dt) <- getNonEmptyValue loc
    case dt of
        Foo envfunction -> evalEnvFun' envfunction tp []  -- todo: obsługa niezaalokowanej pamięci
        otherwise -> return res
    )
-- skip
evalExp' Skip = return $ (Ign, Undefined)
-- overwriting a variable
evalExp' (SAsgn varName exp) = withVar' varName (\loc -> do
    res@(res_tp, res_val) <- evalExp' exp
    (tp, val) <- getValue loc -- checks if variable is allocated
    modify (DMap.insert loc (tp, res_val))
    return res
    )
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
evalExp' (SBegin decl stmt) = do
    store <- get
    env <- lift ask
    let (newEnv, newStore) = declareDecl decl env store
    modify (const newStore)
    local (const newEnv) $ evalExp' stmt
-- Function call
evalExp' (FooCall fooname fooargNames) = withVar' fooname (\loc -> do
    (tp, val) <- getValue loc
    let (Foo envfunction@(env, foo)) = val
    case foo of
        (RawExp rawexp) -> do
            true_foo <- evalExp' rawexp
            case true_foo of
                (tp, (Foo envfunction2)) -> do
                    args <- sequence (map evalExp' fooargNames)
                    evalEnvFun' envfunction2 tp args
                otherwise -> return true_foo
        otherwise -> do
            args <- sequence (map evalExp' fooargNames)
            evalEnvFun' envfunction tp args
    )
-- Function bind
evalExp' (FooBind fooname fooargNames) = withVar' fooname (\loc -> do
    (tp, val) <- getValue loc
    let (Foo envfunction) = val
    args <- sequence (map evalExp' fooargNames)
    evalEnvFun' envfunction tp args
    )

evalExpEither :: Exp -> Either Exception Mementry
evalExpEither exp = runReaderT (evalStateT (evalExp' exp) DMap.empty) []

{- STMT -}
execStoreWithEnv ::  StoreWithEnv a -> Either Exception (a, Store)
execStoreWithEnv mn = runReaderT (runStateT mn DMap.empty) []

execStmt :: Exp -> Either Exception (Mementry, Store)
execStmt stmt = runReaderT (runStateT (evalExp' stmt) DMap.empty) []

execStmtEnv :: Env -> Exp -> Either Exception (Mementry, Store)
execStmtEnv env stmt = runReaderT (runStateT (evalExp' stmt) DMap.empty) env


showStore :: Store -> IO()
showStore = showStoreHelper 0 0

showStoreHelper :: Int -> Int -> Store -> IO()
showStoreHelper counter iter st = do
    if counter == length st
        then return ()
        else case DMap.lookup iter st of
            Nothing -> showStoreHelper counter (iter + 1) st
            Just k -> do
                putStrLn $ (show iter) ++ ": " ++ (show k) ++ "\n"
                showStoreHelper (counter + 1) (iter + 1) st

showState :: Env -> Store -> IO()
showState [] _ = return ()
showState ((varName, varLoc):envrest) store = do
    case DMap.lookup varLoc store of
        Just val -> putStrLn $ varName ++ ": " ++ (show val)
        Nothing -> putStrLn $ varName ++ ": Nothing"
    showState envrest store


{- DECL -}

footypes :: Type -> [Type] -- array consisting of only basic types
footypes (IntT) = [IntT]
footypes (BoolT) = [BoolT]
footypes (FooBr tp) = [tp]
footypes (FooT type1 type2) = (footypes type1) ++ (footypes type2)

fooFromExpVars :: [Var] -> Exp -> Function
fooFromExpVars [] exp = RawExp exp
fooFromExpVars (x:xs) exp =  (ArgFun x) (fooFromExpVars xs exp)

declareDecl' :: Decl -> State (Env, Store) ()
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
    (env, store) <- get
    case lookup fooname env of
        Nothing -> return () -- todo: obsługa błędu, definicja przed deklaracją
        Just loc -> do
            case DMap.lookup loc store of
                 Nothing -> return () -- todo obsługa błędu jw.
                 Just (tp, dt) -> do
                     (env, store) <- get
                     let funEnv = Foo (env, fooFromExpVars vars expr)
                     modify (\(env, store) -> (env, DMap.insert loc (tp, funEnv) store))

declareDecl :: Decl -> Env -> Store -> (Env, Store)
declareDecl decl env store = execState (declareDecl' decl) (env, store)


declareVars' :: Type -> [Var] -> State (Env, Store) ()
declareVars' tp vars = do
    let dcls = map (\(var, varType) -> declareDecl' (FooDcl var varType)) (zip vars (footypes tp))
    sequence_ dcls

declareVars :: Type -> [Var] -> Env -> Store -> (Env, Store)
declareVars tp vars env store = execState (declareVars' tp vars) (env, store)

declareVar :: Type -> Var -> Env -> Store -> (Env, Store) -- todo: bez overheadu w postaci tablicy
declareVar tp var = declareVars tp [var]


stmt_main :: IO()
stmt_main = do
    contents <- getContents
    let abstractSyn = semPBlock $ lanParse $ lanTokens contents
    putStrLn $ show abstractSyn
    putStrLn $  "\n\n" ++ " ---------- "
    let env = [("x", 0), ("y", 1) , ("z", 2)]
    let res = execStoreWithEnv (checkExp' abstractSyn)
    case res of
        Left message -> do
            putStrLn ("Type Error: " ++ message)
        Right (dt, store) ->  do
            let res2 = execStoreWithEnv (evalExp' abstractSyn)
            case res2 of
                Left message -> putStrLn ("Runtime Error: " ++ message)
                Right (dt, store) -> showStore store
    putStrLn $ " ---------- " ++ "\n\n"








