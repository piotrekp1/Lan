module Stmt where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as DMap
import Data.Maybe
import Datatypes
import SemanticDatatypes
import Gramma
import Tokens
import DTCleaner
import Data.Ord


declareEnvVar :: Var -> Loc -> Env -> Env
declareEnvVar var loc env = case lookup var env of
    Nothing -> env ++ [(var, loc)]
    Just val -> overwriteVar var loc env


overwriteVar :: Var -> Loc -> Env -> Env
overwriteVar var loc env = do
    envSet@(var1, val1) <- (var, loc):env
    if(var1 /= var) then return envSet else return (var, loc)

nextLoc :: Store -> Loc
nextLoc = nextLocHelper 0

nextLocHelper :: Int -> Store -> Int
nextLocHelper x mapObj = case DMap.lookup x mapObj of
    Nothing -> x
    Just k -> nextLocHelper (x + 1) mapObj

overwriteMem :: Loc -> (Type, Maybe Datatype) -> Store -> Store
overwriteMem = DMap.insert

multOverwriteMem :: [(Loc, (Type, Maybe Datatype))] -> Store -> Store
multOverwriteMem [] store = store
multOverwriteMem ((loc, newVal):rest) store = overwriteMem loc newVal newStore where
    newStore = multOverwriteMem rest store

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

-- assumes that tp doesnt consist FooT
evalFun' :: Function -> [Type] -> [Datatype]  -> StoreWithEnv Datatype
-- entire specification
evalFun' (RawExp exp) tp args = evalExp' exp
-- partial specification
evalFun' foo@(ArgFun var fooin) (argtp:resttp) [] = do
    env <- ask
    return $ Foo (env, foo)
-- too many arguments
evalFun' (ArgFun var foo)  [] (arg:rest) = error ("too many parameters")
-- many arguments
evalFun' (ArgFun var foo) (argtp:resttp) (arg:rest) = do
    env <- ask
    store <- get
    let (newEnv, newStore) = declareDecl (FooDcl var argtp) env store
    let loc = fromJust $ lookup var newEnv
    modify (overwriteMem loc (argtp, Just arg))
    local (const newEnv) (evalFun' foo resttp rest)


evalEnvFun' :: EnvFunction -> Type -> [Datatype] -> StoreWithEnv Datatype
evalEnvFun' (env, function) tp args = local (const env) (evalFun' function tps args)
    where tps = footypes tp

evalExp' :: Exp -> StoreWithEnv Datatype
-- sama wartosc
evalExp' (EInt b) = return $ Num b
-- zlozenie wyrazen
evalExp' (EOp op exp1 exp2) = do
    retval1 <- evalExp' exp1
    retval2 <- evalExp' exp2
    return $ getOp op retval1 retval2
-- ewaluacja zmiennej
evalExp' (EVar varName) = do
    env <- ask
    case lookup varName env of
        Nothing -> return $ Num 0 -- todo: obsługa niezadeklarwanej zmiennej
        Just loc -> do
             store <- get
             let (tp, val) = store DMap.! loc
             case val of
                 Nothing -> error ("ewaluacja niezdefiniowanej zmiennej: " ++ varName)
                 Just dt -> do
                     case dt of
                         Foo envfunction -> evalEnvFun' envfunction tp []  -- todo: obsługa niezaalokowanej pamięci, refactor tylu casów
                         otherwise -> return dt
-- deklaracja zmiennej lokalnej
{-evalExp' (ELet varName exp1 exp2) = do
    result1 <- evalExp' exp1 -- todo: obsługa niezgodności typów
    env <- ask
    state <- get
    let memLoc = nextLoc state
    modify (DMap.insert memLoc result1)
    a <- local (declareVar varName memLoc) (evalExp' exp2)
    modify (DMap.delete memLoc)
    return a-}
-- skip
evalExp' Skip = return $ Num 0
-- overwriting a variable
evalExp' (SAsgn varName exp) = do
    env <- ask
    case lookup varName env of
        Nothing -> do
            evalExp' Skip -- todo obsługa przypisania do niezadeklarowanej zmiennej
        Just loc -> do
            res <- evalExp' exp
            store <- get
            case DMap.lookup loc store of
                Nothing -> evalExp' Skip -- todo obsługa błędu który nigdy nie powinien mieć miejsca
                Just (tp, mb_data) -> do -- todo: obsługa niezgodności typów z tym co jest w środku
                    modify (DMap.insert loc (tp, Just res))
                    return res
-- if
evalExp' (SIfStmt bexp stmt1 stmt2) = do
    env <- ask
    (BoolD res) <- evalBExp' bexp
    evalExp' $ if res then stmt1 else stmt2
-- while loop
evalExp' loop@(SWhile bexp stmt) = evalExp' (SIfStmt bexp (SScln stmt loop) Skip)
-- Semicolon
evalExp' (SScln stmt1 stmt2) = do
    evalExp' stmt1
    evalExp' stmt2
-- Begin block
evalExp' (SBegin decl stmt) = do
    store <- get
    env <- ask
    let (newEnv, newStore) = declareDecl decl env store
    modify (const newStore)
    local (const newEnv) $ evalExp' stmt
-- Function call
evalExp' (FooCall fooname fooargNames) = do
    env <- ask
    case lookup fooname env of
       Nothing -> do
           evalExp' Skip -- todo: obsługa przypisania do niezadeklarowanej zmiennej
       Just loc -> do
           store <- get --todo: obsługa kiedy to nie jest funkcja
           let (tp, mb_data_envfunction) = store DMap.! loc --todo: obsługa braku w pamięci
           let (Just (Foo envfunction@(env, foo))) = mb_data_envfunction
           case foo of
               (RawExp rawexp) -> do
                   true_foo <- evalExp' rawexp
                   case true_foo of
                       (Foo envfunction2) -> do
                           args <- sequence (map evalExp' fooargNames)
                           evalEnvFun' envfunction2 tp args
                       otherwise -> return true_foo
               otherwise -> do
                   args <- sequence (map evalExp' fooargNames)
                   evalEnvFun' envfunction tp args
-- Function bind
evalExp' (FooBind fooname fooargNames) = do
    env <- ask
    case lookup fooname env of
        Nothing -> do
            evalExp' Skip -- todo: obsługa przypisania do niezadeklarowanej zmiennej
        Just loc -> do
            store <- get --todo: obsługa kiedy to nie jest funkcja
            let (tp, mb_data_envfunction) = store DMap.! loc --todo: obsługa braku w pamięci
            let (Just (Foo envfunction)) = mb_data_envfunction
            args <- sequence (map evalExp' fooargNames)
            evalEnvFun' envfunction tp args


evalExp :: Exp -> Datatype
evalExp exp = fst $ runReader (runStateT (evalExp' exp) DMap.empty) []

evalExp2 :: Exp -> Int
evalExp2 exp = let (Num res) = evalExp exp in res


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
    env <- ask
    case lookup varName env of
        Nothing -> return $ BoolD False -- todo: obsługa niezadeklarwanej zmiennej
        Just loc -> do
            state <- get
            let (tp, (Just dt)) = state DMap.! loc
            return dt -- todo: obsługa niezaalokowanej pamięci
-- ewaluacja porownania
evalBExp' (BCmp ord exp1 exp2) = do
    (Num ret1) <- evalExp' exp1
    (Num ret2) <- evalExp' exp2
    return . BoolD $ (compare ret1 ret2) == ord

evalBExp :: BExp -> Bool
evalBExp bexp = let (BoolD res) = fst $ runReader (runStateT (evalBExp' bexp) DMap.empty) [] in res
{- STMT -}

execStmt :: Exp -> (Datatype, Store)
execStmt stmt = runReader (runStateT (evalExp' stmt) DMap.empty) []

execStmtEnv :: Env -> Exp -> (Datatype, Store)
execStmtEnv env stmt = runReader (runStateT (evalExp' stmt) DMap.empty) env

stateStmt :: Exp -> Store
stateStmt = snd . execStmt

evalStmt :: Exp -> Datatype
evalStmt = fst . execStmt

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
    let newStore = DMap.insert (nextLoc store) (tp, Nothing) store
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
                     modify (\(env, store) -> (env, DMap.insert loc (tp, Just funEnv) store))

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
    showStore $ stateStmt abstractSyn
{-
    let test2 = ELet "x" (EInt 2) (EVar "x")
    let test3 = SBegin  (DDecl "x" (Num 1)) (SAsgn "x" (EInt 6))
    showStore $ stateStmt test3-}









