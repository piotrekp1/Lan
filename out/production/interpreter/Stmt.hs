module Stmt where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as DMap
import Datatypes
import Parser

type Loc = Int

type Env = [(Var, Loc)]
type Store = DMap.Map Loc Datatype

type StoreWithEnv = StateT Store (Reader Env)


declareVar :: Var -> Loc -> Env -> Env
declareVar var loc env = case lookup var env of
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


getIntOp :: Op -> Int -> Int -> Int
getIntOp OpAdd = (+)
getIntOp OpMul = (*)
getIntOp OpSub = (-)
getIntOp OpDiv = (/)


getOp :: Op -> Datatype -> Datatype -> Datatype
getOp op data1 data2 = case data1 of -- todo obsluga nieintow
        (Num int1) -> case data2 of
                    (Num int2) -> Num $ getIntOp op int1 int2


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
        Nothing -> return $ Num 1 -- todo: obsługa niezadeklarwanej zmiennej
        Just loc -> do
            state <- get
            return $ state DMap.! loc -- todo: obsługa niezaalokowanej pamięci
-- deklaracja zmiennej lokalnej
evalExp' (ELet varName exp1 exp2) = do
    result1 <- evalExp' exp1 -- todo: obsługa niezgodności typów
    env <- ask
    state <- get
    let memLoc = nextLoc state
    modify (DMap.insert memLoc result1)
    a <- local (declareVar varName memLoc) (evalExp' exp2)
    modify (DMap.delete memLoc)
    return a

evalExp :: Exp -> Datatype
evalExp exp = fst $ runReader (runStateT (evalExp' exp) DMap.empty) []


{- STMT -}

execStmt' :: Stmt -> StoreWithEnv Datatype
execStmt' Skip = return $ Num 0
-- overwriting a variable
execStmt' (SAsgn varName exp) = do
    env <- ask
    case lookup varName env of
        {-Nothing -> do
            let result = evalExp2 exp
            local (overwriteVar' varName result)-}
        Nothing -> execStmt' Skip -- todo obsługa przypisania do niezadeklarowanej zmiennej
        Just loc -> do
            res <- evalExp' exp
            modify (DMap.insert loc res)
            execStmt' Skip

-- if
execStmt' (SIfStmt exp stmt1 stmt2) = do
    env <- ask
    (Num res) <- evalExp' exp -- todo obsługa błędnego typu, zamiana na bool
    if res == 0 then execStmt' stmt2 else execStmt' stmt1
-- while loop
execStmt' loop@(SWhile exp stmt) = execStmt' (SIfStmt exp stmt Skip)
-- Semicolon
execStmt' (SScln stmt1 stmt2) = do
    execStmt' stmt1
    execStmt' stmt2
-- Begin block
execStmt' (SBegin decl stmt) = do
    store <- get
    env <- ask
    let (newStore, newEnv) = declareDecl decl store env
    modify (const newStore)
    local (const newEnv) $ execStmt' stmt

execStmt :: Stmt -> (Datatype, Store)
execStmt stmt = runReader (runStateT (execStmt' stmt) DMap.empty) []

execStmtEnv :: Env -> Stmt -> (Datatype, Store)
execStmtEnv env stmt = runReader (runStateT (execStmt' stmt) DMap.empty) env

stateStmt :: Stmt -> Store
stateStmt = snd . execStmt

evalStmt :: Stmt -> Datatype
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
    let (Just val) = DMap.lookup varLoc store
    putStrLn $ varName ++ ": " ++ (show val)
    showState envrest store



{- DECL -}

declareDecl :: Decl -> Store -> Env -> (Store, Env)-- todo zamienić na transformaty monad?
-- named declaration
declareDecl (DDecl varName value) store env = -- todo obsługa podwójnych deklaracji
    (newStore, newEnv) where
        newEnv = declareVar varName (nextLoc store) env
        newStore = DMap.insert (nextLoc store) value store
-- semicolon in decl
declareDecl (DScln decl1 decl2) store env =
    let (store1, env1) = declareDecl decl1 store env in
    declareDecl decl2 store1 env1


stmt_main = do
    contents <- getContents
    let abstractSyn = getTree contents

    --putStrLn $ show $ evalExp test3
    showState env $ stateStmt test5



