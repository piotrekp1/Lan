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
semTerm (TOp op term factor) = EOp op (semTerm term) (semFact factor)
semTerm (Factor factor) = semFact factor

semFact :: Factor -> Exp
semFact (Int a) = EInt a
semFact (Var varName) = EVar varName
semFact (Brack pexp) = semPExp pexp


semPBlock :: PBlock -> Exp
semPBlock (PBegin pdecl psntnc) = SBegin (semPDecl pdecl) (semPSntnc psntnc)
semPBlock (PDecl pdecl) = SBegin (semPDecl pdecl) Skip
semPBlock (PSntnc psntnc) = semPSntnc psntnc

semPSntnc :: PSntnc -> Exp
semPSntnc (PSkip) = Skip
semPSntnc (PScln psntnc1 psntnc2) = SScln (semPSntnc psntnc1) (semPSntnc psntnc2)
semPSntnc (PExpFoo pexpfoo) = semPExpFoo pexpfoo


semPExp0 :: PExp0 -> Exp
semPExp0 (PAsgn var pexpfoo) = SAsgn var $ semPExpFoo pexpfoo
semPExp0 (PIfStmt pbexp pexpfoo1 pexpfoo2) = SIfStmt (semPBexp1 pbexp) (semPExpFoo pexpfoo1) (semPExpFoo pexpfoo2)
semPExp0 (PWhile pexp pexpfoo) = SWhile (semPBexp1 pexp) (semPExpFoo pexpfoo)
semPExp0 (PExp pexp) = semPExp pexp
semPExp0 (SntBrack psntnc) = semPSntnc psntnc
semPExp0 (PFooBrack pexpfoo) = semPExpFoo pexpfoo


semPDecl :: PDecl -> Decl
semPDecl (PDSkip) = DSkip
--semPDecl (PSingDecl var typename) = DDecl var $ standardValue . getType $ typename
semPDecl (PSingDecl var typename) = FooDcl var $ semPFooType typename
semPDecl (PDScln pdecl1 pdecl2) = DScln (semPDecl pdecl1) (semPDecl pdecl2)

semPFooType :: PFooType -> Type
semPFooType (PType type_str) = getType type_str
semPFooType (PMltType type_str pfootype) = FooT (getType type_str) (semPFooType pfootype)

semPBexp1 :: BExp1 -> BExp
semPBexp1 (Or bexp1_1 bexp1_2) = BEOp OpOr (semPBexp1 bexp1_1) (semPBexp1 bexp1_2)
semPBexp1 (BExp2 bexp2) = semPBexp2 bexp2

semPBexp2 :: BExp2 -> BExp
semPBexp2 (And bexp1_1 bexp1_2) = BEOp OpAnd (semPBexp2 bexp1_1) (semPBexp2 bexp1_2)
semPBexp2 (BBrack bexp1) = semPBexp1 bexp1
semPBexp2 (BVal bval) = BEBool bval
semPBexp2 (PCmp pcmp) = semPCmp pcmp

semPCmp :: PCmp -> BExp
semPCmp (PCmpExp comp pexp1 pexp2) = BCmp comp (semPExp pexp1) (semPExp pexp2)

translatePFooArgs :: PFooArgs -> [Exp]
translatePFooArgs (PSngArg pexp0) = [semPExp0 pexp0]
translatePFooArgs (PMltArgs pexp0 pfooargs) = (semPExp0 pexp0):(translatePFooArgs pfooargs)

semPExpFoo :: PExpFoo -> Exp
semPExpFoo (PFooCall var pfooargs) = FooCall var (translatePFooArgs pfooargs)
semPExpFoo (PExp0 pexp0) = semPExp0 pexp0

