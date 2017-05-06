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

getValue :: Loc -> StoreWithEnv Mementry
getValue loc = do
    store <- get
    let err_message = "tried to get memory from not DECLARED variable but with set loc. "
    case DMap.lookup loc store of
         Nothing -> err err_message
         Just tp_dt -> return tp_dt

getNonEmptyValue :: Loc -> StoreWithEnv Mementry
getNonEmptyValue loc = do
    res@(tp, dt) <- getValue loc
    let err_message = "tried to get memory from not DEFINED variable. "
    case dt of
        Undefined -> err err_message
        otherwise -> return res


execMonadWithEnvStore :: Env -> Store -> StoreWithEnv a -> Either Exception (a, Store)
execMonadWithEnvStore env store mn = runReaderT (runStateT mn store) env


execStoreWithEnv ::  StoreWithEnv a -> Either Exception (a, Store)
execStoreWithEnv mn = runReaderT (runStateT mn DMap.empty) []


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

