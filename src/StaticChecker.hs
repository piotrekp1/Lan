module StaticChecker where

import Datatypes
import SemanticDatatypes
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Trans.State as TransSt
import qualified Data.Map.Strict as DMap
import Memory
import Utils


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


typeMismatch :: Type -> Type -> String
typeMismatch expected got = "Expected: " ++ show expected ++ ", Got: " ++ show got ++ ". "

assertTrue :: Bool -> String -> StoreWithEnv Type
assertTrue predicate message = if not predicate then err message else return Ign -- todo upewnic się że to nie szkodzi


getTypeLoc :: Loc -> StoreWithEnv Type
getTypeLoc loc = do
    (tp, val) <- getValue loc
    return tp





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
checkExp' (EVar varName) = pure varName >>= getLoc >>= getTypeLoc
-- skip
checkExp' (Skip) = return Ign
-- overwriting a variable
checkExp' (SAsgn varName exp) = do
    loc <- getLoc varName
    res_tp <- checkExp' exp
    tp <- getTypeLoc loc -- checks if variable is allocated
    assertTrue (res_tp == tp) ("Different types in assignment in " ++ varName ++ ". " ++ (typeMismatch tp res_tp))
    return res_tp
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
checkExp' (FooCall fooname args) = pure fooname >>= getLoc >>= getTypeLoc >>= checkFooCallType' fooname args
-- Function bind
checkExp' (FooBind fooname args) = checkExp' (FooCall fooname args)
-- Lambda
checkExp' (SLam (SLamCon var vartype fooexp)) = withDeclaredCheck (FooDcl var vartype) (do
    fooexp_tp <- checkExp' fooexp
    return $ FooT vartype fooexp_tp
    )
-- lambda call
checkExp' (LamCall lam@(SLamCon varname vartype exp) args) = do
    lam_tp <- checkExp' $ SLam lam
    checkFooCallType' "__ lambda __" args lam_tp




checkFooCallType' :: String -> [Exp] -> Type -> StoreWithEnv Type -- name is used only for a error message
checkFooCallType' fooname [] footype = return footype
checkFooCallType' fooname (firstarg:rest) footype = do
    case footype of
        FooT from_tp to_tp -> do
            firstarg_tp <- checkExp' firstarg
            assertTrue (from_tp == firstarg_tp) $
                 "Wrong argument type in call of a function named: " ++ fooname ++
                 " - " ++ (typeMismatch from_tp firstarg_tp)
            checkFooCallType' fooname rest to_tp
        otherwise -> err $ "Too many parameters in a call, variable: " ++ fooname ++ " - but with parameters given. " ++
                           show footype


checkDecl :: Decl -> Env -> Store -> Either Exception (Env, Store)
checkDecl decl env store = execStateT (checkDecl' decl) (env, store)

getStateStoredLoc :: String -> StateT (Env, Store) (Either Exception) Loc
getStateStoredLoc fooname = do
    (env, store) <- TransSt.get
    case lookup fooname env of
        Nothing -> lift . Left $ "definition before declaration of a function: " ++ fooname ++ ". "
        Just loc -> return loc


getNonEmptyStateStoredVar :: String -> StateT (Env, Store) (Either Exception) (Mementry)
getNonEmptyStateStoredVar fooname = do
    loc <- getStateStoredLoc fooname
    (env, store) <- TransSt.get
    case DMap.lookup loc store of
        Nothing -> lift . Left $ "Internal error, function in envrironment but not in memory, function: " ++ fooname
        Just mementry -> return mementry


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
    (tp, dt) <- getNonEmptyStateStoredVar fooname
    (env, store) <- TransSt.get
    let mn = footypeFromExpVars vars tp expr
    case execMonadWithEnvStore env store mn of
        Left message -> lift . Left $ message
        Right (got_type, store) -> if got_type /= tp then lift . Left $ "function types doesnt work"
                                   else return ()


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


--assumes there is no FooT in array
footypeFromArray :: [Type] -> Type
footypeFromArray [t] = t
footypeFromArray (t:rest) = FooT t (footypeFromArray rest)