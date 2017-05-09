module Utils where

import Datatypes
import SemanticDatatypes
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as DMap
import Memory


err :: String -> StoreWithEnv a
err = lift . lift . lift . Left

assertTrue :: Bool -> String -> StoreWithEnv Type
assertTrue predicate message = if not predicate then err message else return Ign -- todo upewnic się że to nie szkodzi

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

-- assumes that element is an array
getArrayEl :: Mementry -> Int -> StoreWithEnv Mementry
getArrayEl (Array tp, (DataArray dt_arr)) ind = do
    let arr_size = length dt_arr
    assertTrue (ind < arr_size) ("index out of range, " ++
                                "array size: " ++ show arr_size ++
                                "index: " ++ show ind)
    return $ (tp, dt_arr !! ind)
getArrayEl mementry ind = err ("internal error: getArrayEl, mementry: " ++ show mementry ++
                               "index: " ++ show ind)




execStoreWithEnv :: StoreWithEnv a -> Either Exception ((a, Store), [String])
execStoreWithEnv monad = runWriterT $ runReaderT (runStateT monad DMap.empty) []

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



replaceEl :: [a] -> Int -> a ->  [a]
replaceEl arr ind newEl = x ++ newEl:ys  where
        (x,_:ys) = splitAt ind arr


replaceNestedEl :: Datatype -> [Int] -> Datatype -> StoreWithEnv Datatype
replaceNestedEl (DataArray arr) (ind:rest_ind) newEl = do
    let len = length arr
    assertTrue (ind < len) ("index out of range, ind: " ++ show ind ++ ", length: " ++ show len)
    new_ith_el <- replaceNestedEl (arr !! ind) rest_ind newEl
    return $ DataArray $ replaceEl arr ind new_ith_el
replaceNestedEl dt [] newEl = return newEl
replaceNestedEl dt inds newEl = err $ "internal error: " ++ show dt ++ "  " ++ show inds ++ "  " ++ show newEl

-- assumes that datatypes is of Array CharT
dataArrayToStr :: [Datatype] -> String
dataArrayToStr [] = []
dataArrayToStr (el:dt_str) = case el of
    CharD ch -> ch:(dataArrayToStr dt_str)

showDataHelper :: Datatype -> String
showDataHelper (Num n) = show n
showDataHelper (BoolD b) =  show b
showDataHelper (CharD c) = [c]
showDataHelper (DataArray []) = "[]"
showDataHelper (DataArray (fst:rest)) = "[" ++ foldl (\str1 -> \str2 -> str1 ++ ", " ++ str2) (showDataHelper fst) (map showDataHelper rest) ++ "]"



showDatatype :: Datatype -> StoreWithEnv Mementry
showDatatype (Undefined) = err "can't cast undefined variable to string"
showDatatype (Foo foo) = err "can't cast function to a string"
showDatatype dt = return $ (Array CharT, DataArray $ map CharD (showDataHelper dt))
