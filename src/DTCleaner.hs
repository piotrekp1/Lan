module DTCleaner where
import ParseDatatypes
import SemanticDatatypes
import Datatypes

standardValue :: Type -> Datatype
standardValue (IntT) = Num 0
standardValue (BoolT) = BoolD False
-- todo: StandardValue dla funkcji

getType :: String -> Type
getType "Int" = IntT
getType "Bool" = BoolT
getType other = error ("getType: " ++ other ++ show(other == "\"Int\""))

semPExp :: PExp -> Exp
semPExp (Let str pexp1 pexp2) = ELet str (semPExp pexp1) (semPExp pexp2)
semPExp (Exp1 pexp) = semExp1 pexp

semExp1 :: Exp1 -> Exp
semExp1 (E1Op op exp1_1 exp1_2) = EOp op (semExp1 exp1_1) (semExp1 exp1_2)
semExp1 (Term term) = semTerm term

semTerm :: Term -> Exp
semTerm (TOp op term pexpfoo) = EOp op (semTerm term) (semPExpFoo pexpfoo)
semTerm (PExpFoo pexpfoo) = semPExpFoo pexpfoo

semFact :: Factor -> Exp
semFact (Value val) = semVal val
semFact (Var varName) = EVar varName
semFact (Brack pexp0) = semPExp0 pexp0
semFact (FFooCall pexpfoo) = semPExpFoo pexpfoo
semFact (BExp1 bexp1) = semPBexp1 bexp1

semVal :: Value -> Exp
semVal (IntP a) = EVal $ (IntT, Num a)
semVal (BoolP a) = EVal $ (BoolT, BoolD a)

semPBlock :: PBlock -> Exp
semPBlock (PBegin pdecl psntnc) = SBegin (semPDecl pdecl) (semPSntnc psntnc)
semPBlock (PDecl pdecl) = SBegin (semPDecl pdecl) Skip
semPBlock (PSntnc psntnc) = semPSntnc psntnc

semPSntnc :: PSntnc -> Exp
semPSntnc (PSkip) = Skip
semPSntnc (PScln psntnc1 psntnc2) = SScln (semPSntnc psntnc1) (semPSntnc psntnc2)
semPSntnc (PExp0 pexp0) = semPExp0 pexp0


semPExp0 :: PExp0 -> Exp
semPExp0 (PAsgn var pexp0) = SAsgn var $ semPExp0 pexp0
semPExp0 (PIfStmt pbexp pexp0_1 pexp0_2) = SIfStmt (semPExp0 pbexp) (semPExp0 pexp0_1) (semPExp0 pexp0_2)
semPExp0 (PWhile pexp pexp0) = SWhile (semPExp0 pexp) (semPExp0 pexp0)
semPExp0 (PExp pexp) = semPExp pexp
semPExp0 (SntBrack psntnc) = semPSntnc psntnc

semPDecl :: PDecl -> Decl
semPDecl (PDSkip) = DSkip
--semPDecl (PSingDecl var typename) = DDecl var $ standardValue . getType $ typename
semPDecl (PSingDecl var typename) = FooDcl var $ semPFooType typename
semPDecl (PDScln pdecl1 pdecl2) = DScln (semPDecl pdecl1) (semPDecl pdecl2)
semPDecl (PFooDef pfooArgNames pexp0) = FooDfn fooName args exp where
            fooName:args = semArgNames pfooArgNames
            exp = semPExp0 pexp0

semPFooType :: PFooType -> Type
semPFooType (PType type_str) = getType type_str
semPFooType (PMltType pfootype1 pfootype2) = FooT (semPFooType pfootype1) (semPFooType pfootype2)
semPFooType (PTypeBrack pfootype) = FooBr $ semPFooType pfootype

semPBexp1 :: BExp1 -> Exp
semPBexp1 (Or bexp1_1 bexp1_2) = EOp OpOr (semPBexp1 bexp1_1) (semPBexp1 bexp1_2)
semPBexp1 (BExp2 bexp2) = semPBexp2 bexp2

semPBexp2 :: BExp2 -> Exp
semPBexp2 (And bexp2_1 bexp2_2) = EOp OpAnd (semPBexp2 bexp2_1) (semPBexp2 bexp2_2)
semPBexp2 (BBrack bexp1) = semPBexp1 bexp1
semPBexp2 (BVal bval) = EVal (BoolT, BoolD bval)
semPBexp2 (PCmp pcmp) = semPCmp pcmp

semPCmp :: PCmp -> Exp
semPCmp (PCmpExp comp pexp0_1 pexp0_2) = EOp comp (semPExp0 pexp0_1) (semPExp0 pexp0_2)

translatePFooArgs :: PFooArgs -> [Exp]
translatePFooArgs (PSngArg pfactor) = [semFact pfactor]
translatePFooArgs (PMltArgs pfactor pfooargs) = (semFact pfactor):(translatePFooArgs pfooargs)

semPExpFoo :: PExpFoo -> Exp
semPExpFoo (PFooCall var pfooargs) = FooCall var (translatePFooArgs pfooargs)
semPExpFoo (PFooBind var (PEmptArgs)) = FooBind var []
semPExpFoo (PFooBind var pfooargs) = FooBind var (translatePFooArgs pfooargs)
semPExpFoo (Factor factor) = semFact factor

semArgNames :: PFooArgNames -> [Var]
semArgNames (PVarName var) = [var]
semArgNames (PVarNames var pfooargnames) = var:(semArgNames pfooargnames)


