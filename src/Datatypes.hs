module Datatypes where

data Op = OpAdd | OpMul | OpSub | OpDiv deriving (Show)
data Datatype = Num Int deriving (Show)

type Var = String
