module Stmt where

import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Trans.State as TransSt
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

withDeclaredCheck :: Decl -> StoreWithEnv a -> StoreWithEnv a
withDeclaredCheck decl prog = do
    store <- get
    env <- lift ask
    let declareRes = checkDecl decl env store
    case declareRes of
        Left message -> err message
        Right (newEnv, newStore) -> do
            modify (const newStore)
            local (const newEnv) prog


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
    cond_tp <- checkExp' bexp
    assertTrue (cond_tp == BoolT) ("Used non-boolean expression in if or while condition. ")
    tp1 <- checkExp' stmt1
    tp2 <- checkExp' stmt2
    assertTrue (tp1 == tp2) ("Expressions in if have different types. ")
    return tp1
-- while loop
checkExp' loop@(SWhile bexp stmt) = checkExp' (SIfStmt bexp stmt stmt) 
-- Semicolon
checkExp' (SScln stmt1 stmt2) = do
    tp1 <- checkExp' stmt1
    tp2 <- checkExp' stmt2
    return $ if tp2 == Ign then tp1 else tp2
-- Begin block
checkExp' (SBegin decl stmt) = withDeclaredCheck decl (checkExp' stmt)
-- Function call
checkExp' (FooCall fooname args) = withVar' fooname (\l -> getTypeLoc l >>= checkFooCallType' fooname args)
-- Function bind
checkExp' (FooBind fooname args) = checkExp' (FooCall fooname args)
-- Lambda
checkExp' (SLam (SLamCon var vartype fooexp)) = withDeclared (FooDcl var vartype) (do
    fooexp_tp <- checkExp' fooexp
    return $ FooT vartype fooexp_tp
    )
-- lambda call
checkExp' (LamCall lam@(SLamCon varname vartype exp) args) = do
    lam_tp <- checkExp' $ SLam lam
    checkFooCallType' "__ lambda __" args lam_tp

unbrackFoo :: Type -> Type -- todo nawiasy na calej funkcji
unbrackFoo (FooBr tp) = tp
unbrackFoo a = a

checkFooCallType' :: String -> [Exp] -> Type -> StoreWithEnv Type -- name is used only for a error message
checkFooCallType' fooname [] footype = return footype
checkFooCallType' fooname (firstarg:rest) footype = do
    case footype of
        FooT from_tp to_tp -> do
            firstarg_tp <- checkExp' firstarg
            assertTrue (unbrackFoo from_tp == unbrackFoo firstarg_tp) $
                 "Wrong argument type in call of a function named: " ++ fooname ++
                 " - " ++ (typeMismatch from_tp firstarg_tp)
            checkFooCallType' fooname rest to_tp
        otherwise -> err $ "Too many parameters in a call, variable: " ++ fooname ++ " - but with parameters given. " ++
                           show footype


-- assumes that there is a function in mementry (doesn't do type checking)
callFunction :: Mementry -> [Exp] -> StoreWithEnv Mementry
callFunction (tp, Foo envfunction@(env, foo)) argexps = do
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
callFunction (tp, (Undefined)) _ = err "call to a function that was not defined"
callFunction mementry argexps = err $ show mementry ++ "\n ---- \n" ++ show argexps

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
evalExp' (SBegin decl stmt) = withDeclared decl (evalExp' stmt)
-- Function call
evalExp' (FooCall fooname argexps) = withVar' fooname (\loc -> do
    foo_mementry <- getValue loc
    callFunction foo_mementry argexps
    )
-- Function bind
evalExp' (FooBind fooname fooargNames) = withVar' fooname (\loc -> do
    (tp, val) <- getValue loc
    let (Foo envfunction) = val
    args <- sequence (map evalExp' fooargNames)
    evalEnvFun' envfunction tp args
    )
-- lambda
evalExp' lambda@(SLam (SLamCon var vartype fooexp)) = do
    env <- ask
    tp <- checkExp' lambda
    let funEnv = Foo (env, fooFromExpVars [var] fooexp)
    return (tp, funEnv)
-- lambda call
evalExp' (LamCall lam@(SLamCon varname vartype exp) argexps) = do
    lam_mementry <- evalExp' $ SLam lam
   -- err $ show argexps ++ " \n -------- \n " ++ show lam_mementry
    callFunction lam_mementry argexps


evalExpEither :: Exp -> Either Exception Mementry
evalExpEither exp = runReaderT (evalStateT (evalExp' exp) DMap.empty) []

{- STMT -}
execStoreWithEnv ::  StoreWithEnv a -> Either Exception (a, Store)
execStoreWithEnv mn = runReaderT (runStateT mn DMap.empty) []

execMonadWithEnvStore :: Env -> Store -> StoreWithEnv a -> Either Exception (a, Store)
execMonadWithEnvStore env store mn = runReaderT (runStateT mn store) env

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

footypeFromExpVars :: [Var] -> Type -> Exp -> StoreWithEnv Type
footypeFromExpVars (varName:varRest) (FooT from_tp to_tp) exp = do
    store <- get
    env <- ask
    let newEnv = declareEnvVar varName (nextLoc store) env
    let newStore = DMap.insert (nextLoc store) (from_tp, Undefined) store
    modify (const newStore)
   -- err $ show from_tp
    to_type_got <- local (const newEnv) (footypeFromExpVars varRest to_tp exp)
    return $ FooT from_tp to_type_got
footypeFromExpVars [] tp exp = checkExp' exp

-- checks typing while declaring
checkDecl' :: Decl -> StateT (Env, Store) (Either Exception)  ()
-- semicolon in decl
checkDecl' (DScln decl1 decl2) = do
    checkDecl' decl1
    checkDecl' decl2
-- skip
checkDecl' (DSkip)  = return ()
-- fun declaration
checkDecl' (FooDcl varName tp) = do
    (env, store) <- get
    let newStore = DMap.insert (nextLoc store) (tp, Undefined) store
    let newEnv = declareEnvVar varName (nextLoc store) env
    modify (const (newEnv, newStore))
-- function definition
checkDecl' (FooDfn fooname vars expr) = do
    (env, store) <- TransSt.get
    case lookup fooname env of
        Nothing -> lift . Left $ "definition before declaration of a function: " ++ fooname ++ ". "
        Just loc -> do
            case DMap.lookup loc store of
                 Nothing -> lift . Left $ "Internal error, function in envrironment but not in memory, function: " ++ fooname
                 Just (tp, dt) -> do
                     (env, store) <- TransSt.get

                     let mn = footypeFromExpVars vars tp expr
                     case execMonadWithEnvStore env store mn of
                         Left message -> lift . Left $ message
                         Right (got_type, store) -> if got_type /= tp then lift . Left $ "function types doesnt work"
                                                    else return ()

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
    (env, store) <- get
    case lookup fooname env of
        Nothing -> lift . Left $ "definition before declaration of a function: " ++ fooname ++ ". "
        Just loc -> do
            case DMap.lookup loc store of
                 Nothing -> lift . Left $ "Internal error, function in envrironment but not in memory"
                 Just (tp, dt) -> do
                     (env, store) <- get
                     let funEnv = Foo (env, fooFromExpVars vars expr)

                     TransSt.modify (\(env, store) -> (env, DMap.insert loc (tp, funEnv) store))

declareDecl :: Decl -> Env -> Store -> Either Exception (Env, Store)
declareDecl decl env store = execStateT (declareDecl' decl) (env, store)

checkDecl :: Decl -> Env -> Store -> Either Exception (Env, Store)
checkDecl decl env store = execStateT (checkDecl' decl) (env, store)

declareVars' :: Type -> [Var] -> StateT (Env, Store) (Either Exception)  ()
declareVars' tp vars = do
    let dcls = map (\(var, varType) -> declareDecl' (FooDcl var varType)) (zip vars (footypes tp))
    sequence_ dcls

declareVars :: Type -> [Var] -> Env -> Store -> Either Exception (Env, Store)
declareVars tp vars env store = execStateT (declareVars' tp vars) (env, store)

declareVar :: Type -> Var -> Env -> Store -> Either Exception (Env, Store) -- todo: bez overheadu w postaci tablicy
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








