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
evalInfixType (OpAdd) (Array CharT) (tp) = do
    assertTrue (showable tp) ("tried to concat not showable type: " ++ show tp ++ " with string.")
    return $ Array CharT
evalInfixType (OpAdd) (tp) (Array CharT) = evalInfixType (OpAdd) (Array CharT) tp
evalInfixType (OpAdd) (Array tp1) (Array tp2) = do
    assertTrue (tp1 == tp2) ("tried to concat arrays of different types: " ++
                             "type1: " ++ show tp1 ++
                             ", type2: " ++ show tp2)
    return $ Array tp1
evalInfixType (OpSub) (IntT) (IntT) = return IntT
evalInfixType (OpMul) (IntT) (IntT) = return IntT
evalInfixType (OpMul) (Array tp) (IntT) = return $ Array tp
evalInfixType (OpDiv) (IntT) (IntT) = return IntT
evalInfixType (OpOr) (BoolT) (BoolT) = return BoolT
evalInfixType (OpAnd) (BoolT) (BoolT) = return BoolT
evalInfixType op tp1 tp2 = err $ ("Mismatched types in infix operation " ++ show op ++
     ", arg1 type: " ++ show tp1 ++
     ", arg2 type: " ++ show tp2 ++ ". ")


typeMismatch :: Type -> Type -> String
typeMismatch expected got = "Expected: " ++ show expected ++ ", Got: " ++ show got ++ ". "

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
footypeFromExpVars vars tp exp = do
    err $ "too much parameters in a function definition, excessive variables: " ++ show vars


--assumes there is no FooT in array
footypeFromArray :: [Type] -> Type
footypeFromArray [t] = t
footypeFromArray (t:rest) = FooT t (footypeFromArray rest)

-- checks if arguments types fit the expected types
checkFooCallType' :: [Exp] -> Type -> StoreWithEnv Type -- name is used only for a error message
checkFooCallType' [] footype = return footype
checkFooCallType' (firstarg:rest) footype = do
    case footype of
        FooT from_tp to_tp -> do
            firstarg_tp <- checkExp' firstarg
            assertTrue (from_tp == firstarg_tp) $
                 "Wrong argument type in call of a function" ++
                 " - " ++ (typeMismatch from_tp firstarg_tp)
            checkFooCallType' rest to_tp
        otherwise -> err $ "Too many parameters in a call" ++ " - but with parameters given. " ++
                           show footype ++ "arg: " ++ show firstarg


-- syntactic sugar for declaring variable and checking new exp with it
withDeclaredCheck :: Decl -> StoreWithEnv a -> StoreWithEnv a
withDeclaredCheck decl prog = do
    newEnv <- checkDecl' decl
    local (const newEnv) prog


checkTypeCalled :: [Exp] -> Type -> StoreWithEnv Type
checkTypeCalled [] tp = return tp
checkTypeCalled (exp_ind:rest) tp = do
    ind_tp <- checkExp' exp_ind
    case tp of
        Array in_tp -> case ind_tp of
            IntT -> checkTypeCalled rest in_tp
            otherwise -> err "arrays must be indexed with integers."
        otherwise -> err "a array call to something that is not an array"

checkExp' :: Exp -> StoreWithEnv Type
-- just a value
checkExp' (EVal (tp, dt)) = return tp
-- infix operation
checkExp' (EOp op exp1 exp2) = do
    type1 <- checkExp' exp1
    type2 <- checkExp' exp2
    evalInfixType op type1 type2
-- array inplace
checkExp' (EArrDef []) = return $ Array AnyT
checkExp' (EArrDef (fst_exp:rest_exps)) = do
    fst_tp <- checkExp' fst_exp
    (Array rest_tp) <- checkExp' $ EArrDef rest_exps
    assertTrue (fst_tp == rest_tp) ("not consistent types in an array")
    return $ Array fst_tp
-- variable
checkExp' (EMementry (Variable varName)) = getType varName
checkExp' (EMementry (ArrayEl varName ind_exps)) = getType varName >>= checkTypeCalled ind_exps
-- skip
checkExp' (Skip) = return Ign
-- overwriting a variable
checkExp' (SAsgn (Variable varName) exp) = do
    tp <- getType varName
    res_tp <- checkExp' exp
    assertTrue (res_tp == tp) ("Different types in assignment in " ++ varName ++ ". " ++ (typeMismatch tp res_tp))
    return res_tp
-- overwriting array el
checkExp' (SAsgn (ArrayEl varName ind_exps) exp) = do
    arr_tp <- getType varName
    tp <- checkTypeCalled ind_exps arr_tp
    res_tp <- checkExp' exp
    assertTrue (tp == res_tp) ("Different types in assignment in " ++ varName ++ ". " ++ (typeMismatch tp res_tp))
    return tp
-- if
checkExp' (SIfStmt bexp stmt1 stmt2) = do
    cond_tp <- checkExp' bexp
    assertTrue (cond_tp == BoolT) ("Used non-boolean expression in \"if\" or \"while\" condition. ")
    tp1 <- checkExp' stmt1
    tp2 <- checkExp' stmt2
    assertTrue (tp1 == tp2) ("Expressions in \"if\" have different types. ")
    return tp1
-- while loop
checkExp' loop@(SWhile bexp stmt) = checkExp' (SIfStmt bexp stmt stmt)
-- Semicolon
checkExp' (SScln stmt1 stmt2) = do
    tp1 <- checkExp' stmt1
    tp2 <- checkExp' stmt2
    return $ if tp2 == Ign then tp1 else tp2
-- Begin block
checkExp' (SEBegin exp1 exp2) = do
    tp1 <- checkExp' exp1
    tp2 <- checkExp' exp2
    return $ if tp2 == Ign then tp1 else tp2
checkExp' (SDBegin decl stmt) = withDeclaredCheck decl (checkExp' stmt)
-- Function call
checkExp' (FooCall fooexp argexp) = checkExp' fooexp >>= checkFooCallType' [argexp]
-- Lambda
checkExp' (SLam var vartype fooexp) = withDeclaredCheck (FooDcl var vartype) (checkExp' fooexp >>= return . (FooT vartype))
-- emptyLambda
checkExp' (SEmptyLam fooexp) = checkExp' fooexp
-- array call
checkExp' (EArrCall arrexp argexp) = do
    arrType <- checkExp' arrexp
    indType <- checkExp' argexp
    case arrType of
        Array tp -> do
            assertTrue (indType == IntT) "Arrays must be indexed with integers."
            return tp
        otherwise -> err "tried to called something that is not an array."
-- in place operator (++, --)
checkExp' (OpMod eMemEntry op) = do
    checkExp' (SAsgn eMemEntry (EOp op (EMementry eMemEntry) (EVal (IntT, Num 1))))
    checkExp' (EMementry eMemEntry)
-- predeffoo call
checkExp' (EPreDefFoo (Length) arr_exp) = do
    tp <- checkExp' arr_exp
    case tp of
        Array tp -> return IntT
        otherwise -> err "Size needs an array as argument."
checkExp' (EPreDefFoo (Print) str_exp) = do
    tp <- checkExp' str_exp
    case tp of
        Array CharT -> return Ign
        otherwise -> err "print takes string as argument"
checkExp' (EPreDefFoo (ShowFoo) str_exp) = do
     tp <- checkExp' str_exp
     case tp of
         FooT tp1 tp2 -> err "can't cast function to a string"
         Ign -> err "can't cast Ign to a string"
         otherwise -> return $ Array CharT
