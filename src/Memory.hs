module Memory where

import Datatypes
import SemanticDatatypes


import qualified Data.Map.Strict as DMap

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

overwriteMem :: Loc -> (Type, Datatype) -> Store -> Store
overwriteMem = DMap.insert

multOverwriteMem :: [(Loc, (Type, Datatype))] -> Store -> Store
multOverwriteMem [] store = store
multOverwriteMem ((loc, newVal):rest) store = overwriteMem loc newVal newStore where
    newStore = multOverwriteMem rest store