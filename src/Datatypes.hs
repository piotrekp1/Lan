module Datatypes where

data Op = OpAdd | OpMul | OpSub | OpDiv | OpOr | OpAnd deriving (Show)
data Datatype
    = Num Int
    | BoolD Bool
    deriving (Show)

type Var = String
