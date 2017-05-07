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


checkDecl' :: Decl -> StoreWithEnv Env
-- semicolon in decl
checkDecl' (DScln decl1 decl2) = do
    newEnv <- checkDecl' decl1
    local (const newEnv) $ checkDecl' decl2
-- skip
checkDecl' (DSkip) = lift ask
-- fun declaration
checkDecl' (FooDcl varName tp) = declareVar varName tp
-- function definition
checkDecl' (FooDfn fooname vars expr) = do
    tp <- getType fooname
    got_tp <- footypeFromExpVars vars tp expr
    assertTrue (got_tp == tp) $ "Function: " ++ fooname ++ " declared type doesn't match the defined type."
    lift ask


footypeFromExpVars :: [Var] -> Type -> Exp -> StoreWithEnv Type
footypeFromExpVars (varName:varRest) (FooT from_tp to_tp) exp = do
    newEnv <- declareVar varName from_tp
    to_type_got <- local (const newEnv) (footypeFromExpVars varRest to_tp exp)
    return $ FooT from_tp to_type_got
footypeFromExpVars [] tp exp = checkExp' exp


--assumes there is no FooT in array
footypeFromArray :: [Type] -> Type
footypeFromArray [t] = t
footypeFromArray (t:rest) = FooT t (footypeFromArray rest)


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


withDeclaredCheck :: Decl -> StoreWithEnv a -> StoreWithEnv a
withDeclaredCheck decl prog = do
    newEnv <- checkDecl' decl
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
checkExp' (EVar varName) = getType varName
-- skip
checkExp' (Skip) = return Ign
-- overwriting a variable
checkExp' (SAsgn varName exp) = do
    tp <- getType varName
    res_tp <- checkExp' exp
    assertTrue (res_tp == tp) ("Different types in assignment in " ++ varName ++ ". " ++ (typeMismatch tp res_tp))
    return res_tp
-- if
checkExp' (SIfStmt bexp stmt1 stmt2) = do
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
checkExp' (FooCall fooname args) = getType fooname >>= checkFooCallType' fooname args
-- Function bind
checkExp' (FooBind fooname args) = checkExp' (FooCall fooname args)
-- Lambda
checkExp' (SLam (SLamCon var vartype fooexp)) = withDeclaredCheck (FooDcl var vartype) (checkExp' fooexp >>= return . (FooT vartype))
-- lambda call
checkExp' (LamCall lam@(SLamCon varname vartype exp) args) = do
    lam_tp <- checkExp' $ SLam lam
    checkFooCallType' "__ lambda __" args lam_tp


