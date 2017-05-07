module Utils where

import Datatypes
import SemanticDatatypes
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as DMap
import Memory


err :: String -> StoreWithEnv a
err = lift . lift . Left

getLoc :: Var -> StoreWithEnv Loc
getLoc varName = do
    env <- lift ask
    let err_message = "Use of undeclared variable: " ++ varName ++ ". "
    case lookup varName env of
        Nothing -> err err_message
        Just loc -> return loc

getValue :: Var -> StoreWithEnv Mementry
getValue varName = getLoc varName >>= \loc -> do
    store <- get
    let err_message = "tried to get memory from not DECLARED variable but with set loc. var: " ++ varName ++". "
    case DMap.lookup loc store of
         Nothing -> err err_message
         Just tp_dt -> return tp_dt


getType :: Var -> StoreWithEnv Type
getType varName = do
    (tp, dt) <- getValue varName
    return tp

getNonEmptyValue :: Var -> StoreWithEnv Mementry
getNonEmptyValue varName = do
    res@(tp, dt) <- getValue varName
    let err_message = "tried to get memory from not DEFINED variable. var: " ++ varName ++ ". "
    case dt of
        Undefined -> err err_message
        otherwise -> return res


execStoreWithEnv :: StoreWithEnv a -> Either Exception (a, Store)
execStoreWithEnv monad = runReaderT (runStateT monad DMap.empty) []

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

