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


getIntOp :: Op -> Int -> Int -> Int
getIntOp OpAdd = (+)
getIntOp OpMul = (*)
getIntOp OpSub = (-)
getIntOp OpDiv = div

getBoolOp :: Op -> Bool -> Bool -> Bool
getBoolOp OpOr = (||)
getBoolOp OpAnd = (&&)

getOp :: Op -> Datatype -> Datatype -> Datatype
getOp op data1 data2 = case data1 of -- todo obsluga nieintow
        (Num int1) -> case data2 of
                    (Num int2) -> Num $ getIntOp op int1 int2
        (BoolD bool1) -> case data2 of
                    (BoolD bool2) -> BoolD $ getBoolOp op bool1 bool2

typeMismatch :: Type -> Type -> String
typeMismatch expected got = "Expected: " ++ show expected ++ ", Got: " ++ show got


--assumes there is no FooT in array
footypeFromArray :: [Type] -> Type
footypeFromArray [t] = t
footypeFromArray (t:rest) = FooT t (footypeFromArray rest)

-- assumes that tp doesnt consist FooT
evalFun' :: Function -> [Type] -> [(Type, Datatype)]  -> StoreWithEnv (Type, Datatype)
-- entire specification
evalFun' (RawExp exp) tp args = evalExp' exp
-- partial specification
evalFun' foo@(ArgFun var fooin) tp@(argtp:resttp) [] = do
    env <- lift ask
    return $ (footypeFromArray tp, Foo (env, foo))
-- too many arguments
evalFun' (ArgFun var foo)  [] (arg:rest) = fail ("too many parameters")
-- many arguments
evalFun' (ArgFun var foo) (argtp_shouldbe:resttp) ((argtp, arg):rest) = do
    env <- lift ask
    assertTrue (argtp_shouldbe == argtp) ("Wrong parameter type, " ++ typeMismatch argtp_shouldbe argtp)
    store <- get
    let (newEnv, newStore) = declareDecl (FooDcl var argtp) env store
    let loc = fromJust $ lookup var newEnv
    modify (overwriteMem loc (argtp, arg))
    local (const newEnv) (evalFun' foo resttp rest)


evalEnvFun' :: EnvFunction -> Type -> [(Type, Datatype)] -> StoreWithEnv (Type,Datatype)
evalEnvFun' (env, function) tp args = local (const env) (evalFun' function tps args)
    where tps = footypes tp

withVar' :: Var -> (Loc -> StoreWithEnv (Type, Datatype)) -> StoreWithEnv (Type, Datatype)
withVar' varName ifThereIs = do
    env <- lift ask
    let fail_message = "use of undeclaredVariable: " ++ varName
    case lookup varName env of
        Nothing -> fail fail_message
        Just loc -> ifThereIs loc

getValue :: Loc -> StoreWithEnv (Type, Datatype) -- todo: typechecking
getValue loc = do
    store <- get
    let fail_message = "tried to get memory from not DECLARED variable but with set loc"
    case DMap.lookup loc store of
         Nothing -> fail fail_message
         Just tp_dt -> return tp_dt

getNonEmptyValue :: Loc -> StoreWithEnv (Type, Datatype)
getNonEmptyValue loc = do
    res@(tp, dt) <- getValue loc
    let fail_message = "tried to get memory from not DEFINED variable"
    case dt of
        Undefined -> fail fail_message
        otherwise -> return res

assertTrue :: Bool -> String -> StoreWithEnv (Type, Datatype)
assertTrue predicate message = if not predicate then fail message else return (Ign, Undefined) -- todo upewnic się że to nie szkodzi

evalExp' :: Exp -> StoreWithEnv (Type, Datatype)
-- sama wartosc
evalExp' (EInt b) = return $ (IntT, Num b)
-- zlozenie wyrazen
evalExp' (EOp op exp1 exp2) = do
    (tp1, retval1) <- evalExp' exp1    -- todo sprawdzenie typu, zrobienie generycznego getOp
    (tp2, retval2) <- evalExp' exp2
    return $ (IntT, getOp op retval1 retval2) -- todo: dedukcja typu
-- ewaluacja zmiennej
evalExp' (EVar varName) = withVar' varName (\loc -> do
    res@(tp, dt) <- getNonEmptyValue loc
    case dt of
        Foo envfunction -> evalEnvFun' envfunction tp []  -- todo: obsługa niezaalokowanej pamięci, refactor tylu casów
        otherwise -> return res
    )
-- skip
evalExp' Skip = return $ (Ign, Undefined) -- todo ignore element
-- overwriting a variable
evalExp' (SAsgn varName exp) = withVar' varName (\loc -> do
    res@(res_tp, res_val) <- evalExp' exp   -- todo kontrola typu!
    (tp, val) <- getValue loc -- checks if variable is allocated
    assertTrue (res_tp == tp) ("While assigning to " ++ varName ++ " - " ++ (typeMismatch tp res_tp))
    modify (DMap.insert loc (tp, res_val))
    return res
    )
-- if
evalExp' (SIfStmt bexp stmt1 stmt2) = do
    env <- lift ask
    (BoolD res) <- evalBExp' bexp
    evalExp' $ if res then stmt1 else stmt2
-- while loop
evalExp' loop@(SWhile bexp stmt) = evalExp' (SIfStmt bexp (SScln stmt loop) Skip)
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
    -- assertTrue (tp ==  ) todo: obsługa typu
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
    (tp, val) <- getValue loc -- todo: obsługa typu
    let (Foo envfunction) = val
    args <- sequence (map evalExp' fooargNames)
    evalEnvFun' envfunction tp args
    )

{-

evalExp :: Exp ->  Datatype
evalExp exp = fst $ runReaderT (runStateT (evalExp' exp) DMap.empty) []
-}

evalExpEither :: Exp -> Either Exception (Type, Datatype)
evalExpEither exp = runReaderT (evalStateT (evalExp' exp) DMap.empty) []
{-

evalExp2 :: Exp -> Int
evalExp2 exp = let (Num res) = evalExp exp in res

-}

evalBExp' :: BExp -> StoreWithEnv Datatype
-- sama wartosc
evalBExp' (BEBool b) = return $ BoolD b
-- zlozenie wyrazen
evalBExp' (BEOp bop bexp1 bexp2) = do
    retval1 <- evalBExp' bexp1
    retval2 <- evalBExp' bexp2
    return $ getOp bop retval1 retval2
-- ewaluacja zmiennej
evalBExp' (BEVar varName) = do
    env <- lift ask
    case lookup varName env of
        Nothing -> return $ BoolD False -- todo: obsługa niezadeklarwanej zmiennej
        Just loc -> do
            state <- get
            let (tp, dt) = state DMap.! loc
            return dt -- todo: obsługa niezaalokowanej pamięci
-- ewaluacja porownania
evalBExp' (BCmp ord exp1 exp2) = do
    (tp1 ,(Num ret1)) <- evalExp' exp1
    (tp2 ,(Num ret2)) <- evalExp' exp2
    assertTrue (tp1 == tp2) "Comparision between different types"
    return . BoolD $ (compare ret1 ret2) == ord
{-

evalBExp :: BExp -> Bool
evalBExp bexp = let (BoolD res) = fst $ runReader (runStateT (evalBExp' bexp) DMap.empty) [] in res
-}
{- STMT -}

execStmt :: Exp -> Either Exception ((Type, Datatype), Store)
execStmt stmt = runReaderT (runStateT (evalExp' stmt) DMap.empty) []

execStmtEnv :: Env -> Exp -> Either Exception ((Type, Datatype), Store)
execStmtEnv env stmt = runReaderT (runStateT (evalExp' stmt) DMap.empty) env
{-

stateStmt :: Exp -> Store
stateStmt = snd . execStmt

evalStmt :: Exp -> Datatype
evalStmt = fst . execStmt
-}

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

stmt_main = do
    contents <- getContents
    let abstractSyn = semPBlock $ lanParse $ lanTokens contents
    putStrLn $ show abstractSyn
    let env = [("x", 0), ("y", 1) , ("z", 2)]
    let res = execStmt abstractSyn
    case res of
        Left message -> putStrLn message
        Right (dt, store) -> showStore store
{-
    let test2 = ELet "x" (EInt 2) (EVar "x")
    let test3 = SBegin  (DDecl "x" (Num 1)) (SAsgn "x" (EInt 6))
    showStore $ stateStmt test3-}









