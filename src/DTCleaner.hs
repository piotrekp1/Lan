module DTCleaner where
import ParseDatatypes
import SemanticDatatypes
import Datatypes

-- Module in the middle, made to separate semantic types from syntactic types
-- translates semantic types (called parseDatatypes) to semantic datatypes


---
-- helper functions --
getType :: String -> Type
getType "Int" = IntT
getType "Bool" = BoolT
getType "Char" = CharT
getType other = error ("getType: " ++ other ++ show(other == "\"Int\""))


preDefFooFromName :: String -> PreDefFoo
preDefFooFromName "print" = Print
preDefFooFromName "length" = Length
preDefFooFromName "show" = ShowFoo
-------------
-- the translation --
-- Exp --

semPBlock :: PBlock -> Exp
semPBlock (PSkip) = Skip
semPBlock (PExp0 pexp0) = semPExp0 pexp0
semPBlock (PDecl pdecl) = SDBegin (semPDecl pdecl) Skip
semPBlock (PEBegin pexp0 pblock) = SEBegin (semPExp0 pexp0) (semPBlock pblock)
semPBlock (PDBegin pdecl pblock) = SDBegin (semPDecl pdecl) (semPBlock pblock)


semPExp0 :: PExp0 -> Exp
semPExp0 (PAsgn pmementry pexp0) = SAsgn (semPMementry pmementry) $ semPExp0 pexp0
semPExp0 (PExp1 pexp1) = semPExp1 pexp1
semPExp0 (PModAsgn pmementry opName pexp0) = SAsgn semMementry (EOp op (EMementry semMementry) (semPExp0 pexp0)) where
                                            op = readOp opName
                                            semMementry = semPMementry pmementry

semPMementry :: PMementry -> EMementry
semPMementry (PVar varName) = Variable varName
semPMementry (PArrEntry varName parrInds) = ArrayEl varName (semPArrInds parrInds)


semPArrInds :: PArrIndexes -> [Exp]
semPArrInds (PSngInd pexp0) = [semPExp0 pexp0]
semPArrInds (PMltInd pexp0 parrInds) = (semPExp0 pexp0):(semPArrInds parrInds)


semPExp1 :: PExp1 -> Exp
semPExp1 (PIf bexp0_1 bexp0_2 bexp0_3) = SIfStmt (semBExp0 bexp0_1) (semBExp0 bexp0_2) (semBExp0 bexp0_3)
semPExp1 (PWhile bexp0_1 pexp0) = SWhile (semBExp0 bexp0_1) (semPExp0 pexp0)
semPExp1 (BExp0 bexp0) = semBExp0 bexp0

semBExp0 :: BExp0 -> Exp
semBExp0 (POr bexp1 bexp0) = EOp OpOr (semBExp1 bexp1) (semBExp0 bexp0)
semBExp0 (BExp1 bexp1) = semBExp1 bexp1

semBExp1 :: BExp1 -> Exp
semBExp1 (PAnd pcmp bexp1) = EOp OpAnd (semPCmp pcmp) (semBExp1 bexp1)
semBExp1 (PCmp pcmp) = semPCmp pcmp


semPCmp :: PCmp -> Exp
semPCmp (PCmpEq pGrOrLess pcmp) = EOp OpEQ (semPGrOrLess pGrOrLess) (semPCmp pcmp)
semPCmp (PGrOrLess pGrOrLess) = semPGrOrLess pGrOrLess

semPGrOrLess :: PGrOrLess -> Exp
semPGrOrLess (PCmpExp comp arExp0_1 arExp0_2 ) = EOp comp (semArExp0 arExp0_1) (semArExp0 arExp0_2)
semPGrOrLess (ArExp0 arExp0) = semArExp0 arExp0

semArExp0 :: ArExp0 -> Exp
semArExp0 (Ar0Op op arExp0 arExp1) = EOp op (semArExp0 arExp0) (semArExp1 arExp1)
semArExp0 (ArExp1 arExp1) = semArExp1 arExp1

semArExp1 :: ArExp1 -> Exp
semArExp1 (Ar1Op op arExp1 pfoocall ) = EOp op (semArExp1 arExp1) (semPFooCall pfoocall)
semArExp1 (PFooCall pfoocall) = semPFooCall pfoocall

semPFooCall :: PFooCall -> Exp
semPFooCall (PFooCallArg foocall factor) = FooCall (semPFooCall foocall) (semFactor factor)
semPFooCall (PreDefFooCallArg fooname factor) = EPreDefFoo (preDefFooFromName fooname) (semFactor factor)
semPFooCall (Factor factor) = semFactor factor

semFactor :: Factor -> Exp
semFactor (BrackPExp0 pexp0) = semPExp0 pexp0
semFactor (Value value) = semValue value
semFactor (MementryVal pmementry) = EMementry $ semPMementry pmementry
semFactor (PModInPl pmementry opName) = OpMod (semPMementry pmementry) (readOp opName)
semFactor (PBlock pblock) = semPBlock pblock
semFactor (PArrCall arr_factor arg_pexp0) = EArrCall (semFactor arr_factor) (semPExp0 arg_pexp0)

semValue :: Value -> Exp
semValue (IntP a) = EVal $ (IntT, Num a)
semValue (BoolP a) = EVal $ (BoolT, BoolD a)
semValue (CharP a) = EVal $ (CharT, CharD a)
semValue (StringP str) = EVal $ (Array CharT, DataArray $ map CharD str)
semValue (ArrayP arrdata) = EArrDef $ semArrData arrdata
semValue (PLambda var vartype fooexp) = SLam var (semPFooType vartype) (semPExp0 fooexp)
semValue (PEmptyLambda fooexp) = SEmptyLam $ semPExp0 fooexp

semArrData :: ArrData -> [Exp]
semArrData (ArrNothing) = []
semArrData (ArrEl pexp0) = [semPExp0 pexp0]
semArrData (ArrEls pexp0 arrdata) = (semPExp0 pexp0):(semArrData arrdata)

semPFooType :: PFooType -> Type
semPFooType (PMltType ptype pfootype) = FooT (semPType ptype) (semPFooType pfootype)
semPFooType (PType ptype) = semPType ptype

semPType :: PType -> Type
semPType (PRawType type_str) = getType type_str
semPType (PTypeBrack pfootype) = semPFooType pfootype
semPType (PTypeArray pfootype) = Array $ semPFooType pfootype

-----
-- Decl --

semPDecl :: PDecl -> Decl
semPDecl (PDSkip) = DSkip
semPDecl (PSingDecl var typename) = FooDcl var $ semPFooType typename
semPDecl (PDScln pdecl1 pdecl2) = DScln (semPDecl pdecl1) (semPDecl pdecl2)
semPDecl (PFooDef pfooArgNames pexp0) = FooDfn fooName args exp where
            fooName:args = semArgNames pfooArgNames
            exp = semPExp0 pexp0


semArgNames :: PFooArgNames -> [Var]
semArgNames (PVarName var) = [var]
semArgNames (PVarNames var pfooargnames) = var:(semArgNames pfooargnames)
