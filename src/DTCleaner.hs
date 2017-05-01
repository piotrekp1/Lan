module DTCleaner where
import ParseDatatypes
import SemanticDatatypes


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


semPStmt :: PStmt -> Stmt
semPStmt (PSkip) = Skip
semPStmt (PAsgn var pexp) = SAsgn var $ semPExp pexp
semPStmt (PScln pstmt1 pstmt2) = SScln (semPStmt pstmt1) (semPStmt pstmt2)
semPStmt (PIfStmt pexp pstmt1 pstmt2) = SIfStmt (semPExp pexp) (semPStmt pstmt1) (semPStmt pstmt2)
semPStmt (PWhile pexp pstmt) = SWhile (semPExp pexp) (semPStmt pstmt)
semPStmt (PBegin pdecl pstmt) = SBegin (semPDecl pdecl) (semPStmt pstmt)


semPDecl :: PDecl -> Decl
semPDecl (PDSkip) = DSkip
semPDecl (PDecl var datatype) = DDecl var datatype
semPDecl (PDScln pdecl1 pdecl2) = DScln (semPDecl pdecl1) (semPDecl pdecl2)