module Datatypes where
--import SemanticDatatypes

data Op = OpAdd | OpMul | OpSub | OpDiv | OpOr | OpAnd | OpEQ | OpLT | OpGT deriving (Show)
type Var = String
