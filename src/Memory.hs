module Memory where

import Datatypes
import SemanticDatatypes
import qualified Data.Map.Strict as DMap
import Control.Monad.Reader
import Control.Monad.State

declareVar :: Var -> Type -> StoreWithEnv Env
declareVar var tp = do
    env <- lift ask
    newLoc <- nextLoc
    modify $ DMap.insert newLoc (tp, Undefined)
    case lookup var env of
        Nothing -> return $ env ++ [(var, newLoc)]
        Just val -> overwriteVar var newLoc


overwriteVar :: Var -> Loc -> StoreWithEnv Env
overwriteVar var loc = do
    env <- ask
    return [ if(var1 == var) then (var, loc) else envEl | envEl@(var1 ,loc1) <- env ]

nextLoc :: StoreWithEnv Loc
nextLoc = do
    store <- get
    return $ nextLocHelper 0 store


nextLocHelper :: Int -> Store -> Int
nextLocHelper x mapObj = case DMap.lookup x mapObj of
    Nothing -> x
    Just k -> nextLocHelper (x + 1) mapObj

overwriteMem :: Loc -> (Type, Datatype) -> Store -> Store
overwriteMem = DMap.insert

multOverwriteMem :: [(Loc, (Type, Datatype))] -> Store -> Store
multOverwriteMem [] store = store
multOverwriteMem ((loc, newVal):rest) store = overwriteMem loc newVal newStore where
    newStore = multOverwriteMem rest store