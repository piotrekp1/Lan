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
