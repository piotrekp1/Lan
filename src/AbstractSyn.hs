module AbstractSyn where
import Datatypes
import SemanticDatatypes
import Gramma
import Tokens
import DTCleaner

syn = do
    contents <- getContents
    let abstractSyn = semPBlock $ lanParse $ lanTokens contents
    putStrLn $ show $ abstractSyn
