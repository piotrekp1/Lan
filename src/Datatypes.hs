module Datatypes where
--import SemanticDatatypes

data Op = OpAdd | OpMul | OpSub | OpDiv | OpOr | OpAnd | OpEQ | OpLT | OpGT deriving (Show)

readOp :: String -> Op
readOp "Add" = OpAdd
readOp "Mul" = OpMul
readOp "Sub" = OpSub
readOp "Div" = OpDiv

type Var = String
